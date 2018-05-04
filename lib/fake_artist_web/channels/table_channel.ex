defmodule FakeArtistWeb.TableChannel do
  use Phoenix.Channel

  def join("table:" <> table_name, %{}, socket) do
    send(self(), {:after_join, table_name})
    {:ok, socket}
  end

  def handle_info({:after_join, table_name}, socket) do
    table_pid = FakeArtist.Hostess.get_table_pid(table_name)
    FakeArtist.Table.add_self(table_pid)
    socket = assign(socket, :table, table_pid)
    {:noreply, socket}
  end

  def handle_in("name_tag", %{"name" => name}, socket) do
    FakeArtist.Table.update_name_tag(socket.assigns.table, {socket.assigns.id, name})
    {:noreply, socket}
  end
end
