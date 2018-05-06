defmodule FakeArtist.Player do
  defstruct(id: nil, name: nil, is_active: nil, seat: nil)
end

defmodule FakeArtist.Table do
  use GenServer

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

  # Server Callbacks
  def init([name]) do
    {:ok, {%{}, name, 0, nil}}
  end

  def handle_call(
        {:update_name_tag, {id, name_tag}},
        _from,
        {id_map, table_name, user_count, state_pid}
      ) do
    id_map = Map.put(id_map, id, name_tag)

    FakeArtistWeb.Endpoint.broadcast("table:#{table_name}", "update", %{
      state: %{names: id_map}
    })

    {:reply, :ok, {id_map, table_name, user_count, state_pid}}
  end

  def handle_call(:add_self, {from_pid, _}, {id_map, table_name, user_count, state_pid}) do
    IO.puts("started monitoring #{inspect(from_pid)}")
    Process.monitor(from_pid)
    IO.puts("player count increased from #{user_count} to #{user_count + 1}")
    {:reply, :ok, {id_map, table_name, user_count + 1, state_pid}}
  end

  def handle_call(:start_game, _from, {id_map, table_name, user_count, _old_state_pid}) do
    {:ok, state_pid} = Map.keys(id_map) |> FakeArtist.Game.start_link()

    {:ok, %{seats: seats, active_seat: active_seat}} = FakeArtist.Game.get_state(state_pid)

    players = assemble_players(seats, id_map, active_seat)

    FakeArtistWeb.Endpoint.broadcast("table:#{table_name}", "update_game", %{
      players: players
    })

    {:reply, :ok, {id_map, table_name, user_count, state_pid}}
  end

  def handle_info(
        {:DOWN, _ref, :process, _from, _reason},
        {id_map, table_name, user_count, _old_state_pid}
      ) do
    IO.puts("table lost connection.")
    IO.puts("player count decreased from #{user_count} to #{user_count - 1}")
    user_count = user_count - 1

    if user_count == 0 do
      IO.puts("suicide")
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
end
