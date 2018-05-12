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
    name: ""
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
    id = to_string(id)

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

    {game_master_id, player_ids_without_game_master} = get_random_player(player_ids)
    {trickster_id, _} = get_random_player(player_ids_without_game_master)

    players =
      update_player_role(players, game_master_id, :game_master)
      |> update_player_role(trickster_id, :trickster)

    game_master = Map.get(players, game_master_id) |> Map.put(:seat, 0)
    players_without_game_master = Map.delete(players, game_master_id)

    seats = 1..((players |> Map.keys() |> length()) - 1) |> Enum.to_list() |> Enum.shuffle()
    players_with_seats = Enum.zip(seats, players_without_game_master)

    players =
      for(
        {index, {player_id, player}} <- players_with_seats,
        into: %{},
        do: {player_id, Map.put(player, :seat, index)}
      )
      |> Map.new()

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
        remaining_turns: ((players |> Map.keys() |> length()) - 1) * 2,
        little_state: :draw
    }

    FakeArtistWeb.Endpoint.broadcast("table:#{table_name}", "update_game", state)

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
    if remaining_turns == 1 do
      Logger.info(fn -> "Voting." end)

      state = %{
        state
        | little_state: :vote
      }

      {:reply, :ok, state}
    else
      state = %{
        state
        | active_players: get_active_players(players, active_players),
          remaining_turns: remaining_turns - 1
      }

      FakeArtistWeb.Endpoint.broadcast("table:#{table_name}", "update_game", state)

      {:reply, :ok, state}
    end
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
      {:stop, :shutdown, %{}}
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

  @spec get_active_players(map(), list()) :: list()
  defp get_active_players(players, active_players) do
    current_active_player_id = active_players |> hd()
    current_active_player_seat = Map.get(players, current_active_player_id).seat
    next_seat = get_next_seat(current_active_player_seat, players |> Map.keys() |> length())
    next_id = players |> Enum.find(fn {_, player} -> player.seat == next_seat end) |> elem(0)
    [next_id]
  end

  @spec get_next_seat(number(), number()) :: number()
  defp get_next_seat(current_seat, player_count) do
    if current_seat + 1 == player_count do
      0
    else
      current_seat + 1
    end
  end

  @spec get_random_player(list()) :: tuple()
  defp get_random_player(players) do
    random_player = Enum.random(players)
    without_random_player = List.delete(players, random_player)
    {random_player, without_random_player}
  end

  @spec list_up_to(number()) :: list()
  def list_up_to(n) do
    Enum.to_list(0..(n - 1))
  end
end
