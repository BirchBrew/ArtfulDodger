defmodule FakeArtistWeb.WelcomeChannel do
  use Phoenix.Channel

  def join("welcome", %{}, socket) do
    {:ok, socket}
  end

  def handle_in("new_table", %{}, socket) do
    {:ok, %{name: name}} = FakeArtist.Hostess.start_new_table()
    {:reply, {:ok, %{table: name, id: socket.assigns.id}}, socket}
  end

  def handle_in("join_table", %{"table" => name}, socket) do
    case FakeArtist.Hostess.join_existing_table(name) do
      {:ok, table_pid} ->
        socket = assign(socket, :table, table_pid)
        {:reply, {:ok, %{table: name, id: socket.assigns.id}}, socket}

      {:error, err} ->
        {:reply, {:error, %{error: err}}, socket}
    end
  end
end
