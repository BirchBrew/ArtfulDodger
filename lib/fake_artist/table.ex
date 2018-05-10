defmodule FakeArtist.Player do
  defstruct(id: nil, name: nil, is_active: nil, seat: nil)
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
    {:ok, {%{}, name, 0}}
  end

  def handle_call(
        {:update_name_tag, {id, name_tag}},
        _from,
        {id_map, table_name, user_count}
      ) do
    id_map = Map.put(id_map, id, name_tag)

    FakeArtistWeb.Endpoint.broadcast("table:#{table_name}", "update", %{names: Map.values(id_map)})

    {:reply, :ok, {id_map, table_name, user_count}}
  end

  def handle_call(:add_self, {from_pid, _}, {id_map, table_name, user_count}) do
    Logger.info(fn -> "started monitoring #{inspect(from_pid)}" end)
    Process.monitor(from_pid)
    Logger.info(fn -> "player count increased from #{user_count} to #{user_count + 1}" end)
    {:reply, :ok, {id_map, table_name, user_count + 1}}
  end

  def handle_call(:start_game, _from, {id_map, table_name, user_count}) do
    players = Map.keys(id_map)

    {game_master, players_without_game_master} = get_random_player(players)
    {trickster, players_without_roles} = get_random_player(players_without_game_master)
    roles = get_roles(game_master, trickster, players_without_roles)
    seats = get_seats(game_master, players_without_game_master)
    active_seat = 0
    players = assemble_players(seats, id_map, active_seat)

    FakeArtistWeb.Endpoint.broadcast("table:#{table_name}", "start_game", %{})
    FakeArtistWeb.Endpoint.broadcast("table:#{table_name}", "update_game", %{players: players})

    {:reply, :ok,
     {id_map, table_name, user_count, %{roles: roles, seats: seats, active_seat: active_seat}}}
  end

  def handle_call(
        :progress_game,
        _from,
        {id_map, table_name, user_count,
         %{roles: roles, seats: seats, active_seat: active_seat, remaining_turns: remaining_turns}}
      ) do
    if remaining_turns == 1 do
      Logger.info(fn -> "Voting." end)

      {:reply, :ok,
       {id_map, table_name, user_count,
        %{
          state: "voting",
          seats: seats,
          active_seats: (seats |> length() |> list_up_to()) -- [0]
        }}}
    else
      next_seat = get_next_seat(seats, active_seat)
      active_seat = next_seat
      remaining_turns = remaining_turns - 1

      players = assemble_players(seats, id_map, active_seat)
      FakeArtistWeb.Endpoint.broadcast("table:#{table_name}", "update_game", %{players: players})

      {:reply, :ok,
       {id_map, table_name, user_count,
        %{
          roles: roles,
          seats: seats,
          active_seat: active_seat,
          remaining_turns: remaining_turns
        }}}
    end
  end

  def handle_call(
        :choose_category,
        _from,
        {id_map, table_name, user_count, %{roles: roles, seats: seats}}
      ) do
    active_seat = 1
    remaining_turns = (length(seats) - 1) * 2

    players = assemble_players(seats, id_map, active_seat)

    FakeArtistWeb.Endpoint.broadcast("table:#{table_name}", "update_game", %{players: players})

    {:reply, :ok,
     {id_map, table_name, user_count,
      %{roles: roles, seats: seats, active_seat: active_seat, remaining_turns: remaining_turns}}}
  end

  def handle_info(
        {:DOWN, _ref, :process, _from, _reason},
        {id_map, table_name, user_count}
      ) do
    Logger.info(fn -> "table lost connection." end)
    Logger.info(fn -> "player count decreased from #{user_count} to #{user_count - 1}" end)

    user_count = user_count - 1

    if user_count == 0 do
      Logger.info(fn -> "suicide" end)
      {:stop, :shutdown, {%{}, "", 0}}
    else
      {:noreply, {id_map, table_name, user_count}}
    end
  end

  @spec assemble_players(list(), map(), number()) :: list()
  def assemble_players(seats, id_map, active_seat) do
    seats
    |> Enum.with_index(0)
    |> Enum.map(fn {id, index} ->
      %FakeArtist.Player{
        id: id,
        name: Map.get(id_map, id),
        seat: index,
        is_active: index == active_seat
      }
    end)
  end

  @spec get_random_player(list()) :: tuple()
  defp get_random_player(players) do
    random_player = Enum.random(players)
    without_random_player = List.delete(players, random_player)
    {random_player, without_random_player}
  end

  @spec get_roles(binary(), binary(), list()) :: map()
  defp get_roles(game_master, trickster, players) do
    roles = %{game_master => :game_master, trickster => :trickster}
    roles = for player <- players, into: roles, do: {player, :player}
    roles
  end

  @spec get_seats(binary(), list()) :: list()
  defp get_seats(game_master, players) do
    [game_master | Enum.shuffle(players)]
  end

  @spec list_up_to(number()) :: list()
  def list_up_to(n) do
    Enum.to_list(0..(n - 1))
  end

  @spec get_next_seat(list(), number()) :: number()
  defp get_next_seat(players, active_seat) do
    if active_seat + 1 == length(players) do
      1
    else
      active_seat + 1
    end
  end
end
