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

  def handle_in("start_game", _, socket) do
    FakeArtist.Table.start_game(socket.assigns.table)
    {:noreply, socket}
  end

  def handle_in("progress_game", _, socket) do
    FakeArtist.Table.progress_game(socket.assigns.table)
    {:noreply, socket}
  end

  def handle_in("choose_category", _, socket) do
    FakeArtist.Table.choose_category(socket.assigns.table)
    {:noreply, socket}
  end

  def handle_in("vote_for", %{"for" => voted_for}, socket) do
    FakeArtist.Table.vote_for(socket.assigns.table, {voted_for, socket.assigns.id})
    {:noreply, socket}
  end
end
