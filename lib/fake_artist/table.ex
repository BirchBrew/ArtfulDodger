defmodule FakeArtist.State do
  defstruct(
    big_state: :lobby,
    little_state: :pick,
    topic: nil,
    category: nil,
    active_players: [],
    winner: nil,
    players: %{},
    table_name: nil,
    remaining_turns: 0,
    connected_computers: 0
  )
end

defmodule FakeArtist.Player do
  defstruct(
    seat: nil,
    role: :player,
    name: "",
    voted_for: nil
  )
end

defmodule FakeArtist.Table do
  use GenServer

  require Logger

  # Public API
  def start_link(default) do
    GenServer.start_link(__MODULE__, default)
  end

  def update_name_tag(pid, {id, name_tag}) do
    GenServer.call(pid, {:update_name_tag, {id, name_tag}})
  end

  def add_self(pid) do
    GenServer.call(pid, :add_self)
  end

  def start_game(pid) do
    GenServer.call(pid, :start_game)
  end

  def progress_game(pid) do
    GenServer.call(pid, :progress_game)
  end

  def choose_category(pid) do
    GenServer.call(pid, :choose_category)
  end

  def vote_for(pid, {voted_for, voted_by}) do
    GenServer.call(pid, {:vote_for, {voted_for, voted_by}})
  end

  # Server Callbacks
  def init([name]) do
    {:ok, %FakeArtist.State{table_name: name}}
  end

  def handle_call(
        {:update_name_tag, {id, name_tag}},
        _from,
        state = %{
          players: players,
          table_name: table_name
        }
      ) do
    players =
      if Map.has_key?(players, id) do
        players
      else
        Map.put(players, id, %FakeArtist.Player{})
      end

    players = update_player_name(players, id, name_tag)

    state = %{state | players: players}

    FakeArtistWeb.Endpoint.broadcast("table:#{table_name}", "update", state)

    {:reply, :ok, state}
  end

  def handle_call(:add_self, {from_pid, _}, state = %{connected_computers: connected_computers}) do
    Logger.info(fn -> "started monitoring #{inspect(from_pid)}" end)
    Process.monitor(from_pid)

    Logger.info(fn ->
      "player count increased from #{connected_computers} to #{connected_computers + 1}"
    end)

    {:reply, :ok, %{state | connected_computers: connected_computers + 1}}
  end

  def handle_call(
        :start_game,
        _from,
        state = %{
          players: players,
          table_name: table_name
        }
      ) do
    player_ids = Map.keys(players)

    [game_master_id, trickster_id | _] = Enum.shuffle(player_ids)

    players =
      update_player_role(players, game_master_id, :game_master)
      |> update_player_role(trickster_id, :trickster)

    game_master = Map.get(players, game_master_id) |> Map.put(:seat, 0)
    players_without_game_master = Map.delete(players, game_master_id)

    last_seat_num = players_without_game_master |> Enum.count()

    seats = 1..last_seat_num |> Enum.to_list() |> Enum.shuffle()
    players_with_seats = Enum.zip(seats, players_without_game_master)

    players =
      for(
        {index, {player_id, player}} <- players_with_seats,
        into: %{},
        do: {player_id, Map.put(player, :seat, index)}
      )

    players = players |> Map.put(game_master_id, game_master)

    state = %{
      state
      | big_state: :game,
        little_state: :pick,
        active_players: [game_master_id],
        players: players
    }

    FakeArtistWeb.Endpoint.broadcast("table:#{table_name}", "update", state)

    {:reply, :ok, state}
  end

  def handle_call(
        :choose_category,
        _from,
        state = %{
          active_players: active_players,
          players: players,
          table_name: table_name
        }
      ) do
    state = %{
      state
      | active_players: get_active_players(players, active_players),
        remaining_turns: ((players |> Enum.count()) - 1) * 2,
        little_state: :draw
    }

    FakeArtistWeb.Endpoint.broadcast("table:#{table_name}", "update", state)

    {:reply, :ok, state}
  end

  def handle_call(
        :progress_game,
        _from,
        state = %{
          active_players: active_players,
          players: players,
          remaining_turns: remaining_turns,
          table_name: table_name
        }
      ) do
    state =
      if remaining_turns == 1 do
        Logger.info(fn -> "Voting." end)

        %{
          state
          | little_state: :vote
        }
      else
        %{
          state
          | active_players: get_active_players(players, active_players),
            remaining_turns: remaining_turns - 1
        }
      end

    FakeArtistWeb.Endpoint.broadcast("table:#{table_name}", "update", state)

    {:reply, :ok, state}
  end

  def handle_call(
        {:vote_for, {voted_for, voted_by}},
        _from,
        state = %{
          players: players,
          table_name: table_name
        }
      ) do
    state = %{state | players: update_player_vote(players, voted_by, voted_for)}

    state =
      if state |> everyone_has_voted() do
        Logger.info(fn -> "Game Over." end)

        %{
          state
          | big_state: :end,
            winner: get_winner(players)
        }
      else
        state
      end

    FakeArtistWeb.Endpoint.broadcast("table:#{table_name}", "update", state)

    {:reply, :ok, state}
  end

  def handle_info(
        {:DOWN, _ref, :process, _from, _reason},
        state = %{connected_computers: connected_computers}
      ) do
    Logger.info(fn -> "table lost connection." end)

    Logger.info(fn ->
      "player count decreased from #{connected_computers} to #{connected_computers - 1}"
    end)

    if connected_computers - 1 == 0 do
      Logger.info(fn -> "suicide" end)
      {:stop, :shutdown, state}
    else
      state = %{
        state
        | connected_computers: connected_computers - 1
      }

      {:noreply, state}
    end
  end

  @spec update_player_role(map(), number(), atom()) :: map()
  defp update_player_role(players, id, new_role) do
    player = Map.get(players, id)
    player_with_new_role = %{player | role: new_role}
    Map.put(players, id, player_with_new_role)
  end

  @spec update_player_name(map(), number(), atom()) :: map()
  defp update_player_name(players, id, new_name) do
    player = Map.get(players, id)
    player_with_new_name = %{player | name: new_name}
    Map.put(players, id, player_with_new_name)
  end

  @spec update_player_vote(map(), number(), atom()) :: map()
  defp update_player_vote(players, id, new_vote) do
    player = Map.get(players, id)
    player_with_new_vote = %{player | voted_for: new_vote}
    Map.put(players, id, player_with_new_vote)
  end

  @spec get_active_players(map(), list()) :: list()
  defp get_active_players(players, active_players) do
    current_active_player_id = active_players |> hd()
    current_active_player_seat = Map.get(players, current_active_player_id).seat
    next_seat = get_next_seat(current_active_player_seat, players |> Enum.count())
    {next_id, _} = players |> Enum.find(fn {_, player} -> player.seat == next_seat end)
    [next_id]
  end

  @spec get_next_seat(number(), number()) :: number()
  defp get_next_seat(current_seat, player_count) do
    if current_seat + 1 == player_count do
      1
    else
      current_seat + 1
    end
  end

  @spec everyone_has_voted(map()) :: boolean()
  defp everyone_has_voted(state) do
    game_master_vote = 1

    Enum.count(state.players, fn {_k, v} -> v.voted_for == nil end) - game_master_vote == 0
  end

  @spec get_winner(map()) :: binary()
  defp get_winner(players) do
    # TODO: Figure out who should win ties and make sure the right people win ties.
    map_of_counts =
      Enum.reduce(players, %{}, fn {_player_id, player}, acc ->
        Map.update(acc, player.voted_for, 1, &(&1 + 1))
      end)

    {key, _value} = Enum.max_by(map_of_counts, fn {_k, v} -> v end)

    if key == get_trickster_id(players) do
      "Players win!"
    else
      "Trickster and Game Master win!"
    end
  end

  @spec get_trickster_id(map()) :: binary()
  defp get_trickster_id(players) do
    {trickster_id, _trickster} =
      players |> Map.to_list() |> Enum.find(fn {_id, player} -> player.role == :trickster end)

    trickster_id
  end
end
