defmodule FakeArtist.Hostess do
  use GenServer

  def new do
    GenServer.start_link(__MODULE__, 0)
  end

  def join_existing_table(pid, table_name) do
    GenServer.call(pid, {:join_existing_table, table_name})
  end

  def join_new_table(pid) do
    GenServer.call(pid, :start_new_table)
  end

  def init(args) do
    {:ok, args}
  end

  def start_link(default) do
    GenServer.start_link(__MODULE__, default)
  end

  def handle_call({:join_existing_table, table_name}, _from, tables) do
    case try_join(table_name, tables) do
      {:ok, %{pid: pid, tables: updated_tables}} ->
        {:reply, {:ok, pid}, updated_tables}

      {:error, message} ->
        {:reply, {:error, message}, tables}
    end
  end

  def handle_call(:start_new_table, _from, tables) do
    case try_create(tables) do
      {:ok, %{name: name, pid: pid, tables: updated_tables}} ->
        {:reply, {:ok, %{name: name, pid: pid}}, updated_tables}

      {:error, message} ->
        {:reply, {:error, message}, tables}
    end
  end

  def try_join(table_name, tables) do
    if Map.has_key?(tables, table_name) do
      {:ok, %{pid: Map.get(tables, table_name), tables: tables}}
    else
      {:error, "The room doesn't exist"}
    end
  end

  def generate_new_name() do
    length_of_room_name = 5
    chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
    code_points = String.codepoints(chars)

    Enum.reduce(1..length_of_room_name, [], fn _i, acc ->
      [Enum.random(code_points) | acc]
    end)
    |> Enum.join("")
  end

  def get_unique_name_helper(tables, name \\ "michelangelo", tries \\ 0) do
    unless Map.has_key?(tables, name) do
      {:ok, name}
    else
      if tries > 1000 do
        {:error}
      else
        get_unique_name_helper(tables, generate_new_name(), tries + 1)
      end
    end
  end

  def get_unique_name(tables) do
    get_unique_name_helper(tables)
  end

  def create_process() do
    {:ok, pid} = FakeArtist.DynamicSupervisor.start_child()
    {:ok, pid}
  end

  def try_create(tables) do
    with {:ok, name} <- get_unique_name(tables),
         {:ok, pid} <- create_process(),
         tables = Map.put(tables, name, pid) do
      {:ok, %{name: name, pid: pid, tables: tables}}
    else
      _ -> {:error, "Couldn't create the room"}
    end
  end
end
