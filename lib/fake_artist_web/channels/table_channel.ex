defmodule FakeArtistWeb.TableChannel do
  use Phoenix.Channel

  def join("table:" <> table_name, _, socket) do
    send(self(), {:after_join, table_name})
    {:ok, socket}
  end

  def handle_info({:after_join, table_name}, socket) do
    table_pid = FakeArtist.Hostess.get_table_pid(table_name)
    FakeArtist.Table.add_self(table_pid, socket.assigns.id)
    socket = assign(socket, :table, table_pid)
    {:noreply, socket}
  end

  def handle_in("choose_name", %{"name" => name}, socket) do
    FakeArtist.Table.choose_name(socket.assigns.table, {socket.assigns.id, name})
    {:noreply, socket}
  end

  def handle_in("start_game", _, socket) do
    FakeArtist.Table.start_game(socket.assigns.table)
    {:noreply, socket}
  end

  def handle_in("paint_line", %{"line" => line}, socket) do
    FakeArtist.Table.paint_line(socket.assigns.table, socket.assigns.id, line)
    {:noreply, socket}
  end

  def handle_in("progress_game", %{}, socket) do
    FakeArtist.Table.progress_game(socket.assigns.table, socket.assigns.id)
    {:noreply, socket}
  end

  def handle_in("choose_subject", %{"subject" => subject}, socket) do
    FakeArtist.Table.choose_subject(socket.assigns.table, subject)
    {:noreply, socket}
  end

  def handle_in("vote_for", %{"for" => voted_for}, socket) do
    FakeArtist.Table.vote_for(socket.assigns.table, {voted_for, socket.assigns.id})
    {:noreply, socket}
  end

  def handle_in("guess_subject", %{"subject" => subject}, socket) do
    FakeArtist.Table.guess_subject(socket.assigns.table, subject)
    {:noreply, socket}
  end

  def handle_in("validate_guess", %{"is_correct" => is_correct}, socket) do
    FakeArtist.Table.validate_guess(socket.assigns.table, is_correct)
    {:noreply, socket}
  end
end
