defmodule FakeArtist.Game do
  use GenStateMachine
  require Logger

  @spec start_link(list()) :: tuple()
  def start_link(players) do
    GenStateMachine.start_link(__MODULE__, players)
  end

  def get_state(pid) do
    GenStateMachine.call(pid, :get_state)
  end

  def progress_game(pid) do
    GenStateMachine.call(pid, :player_drew)
  end

  def choose_category(pid) do
    GenStateMachine.call(pid, :game_master_chose)
  end

  # Callbacks
  def init(players) do
    {game_master, players_without_game_master} = get_random_player(players)
    {trickster, players_without_roles} = get_random_player(players_without_game_master)
    roles = get_roles(game_master, trickster, players_without_roles)
    seats = get_seats(game_master, players_without_game_master)

    {:ok, :game_master_chooses_topic, %{roles: roles, seats: seats, active_seat: 0}}
  end

  def handle_event({:call, from}, :game_master_chose, :game_master_chooses_topic, %{
        roles: roles,
        seats: seats
      }) do
    {:next_state, :player_draw,
     %{
       roles: roles,
       seats: seats,
       active_seat: 1,
       remaining_turns: (length(seats) - 1) * 2
     }, [{:reply, from, %{seats: seats, roles: roles, active_seat: 1}}]}
  end

  def handle_event({:call, from}, :player_drew, :player_draw, %{
        roles: roles,
        seats: seats,
        active_seat: active_seat,
        remaining_turns: remaining_turns
      }) do
    if remaining_turns == 1 do
      Logger.info(fn -> "Voting." end)
      {:next_state, :vote, %{roles: roles, seats: seats, active_seat: active_seat}}
    else
      next_seat = get_next_seat(seats, active_seat)

      {:next_state, :player_draw,
       %{roles: roles, seats: seats, active_seat: next_seat, remaining_turns: remaining_turns - 1},
       [{:reply, from, %{seats: seats, roles: roles, active_seat: next_seat}}]}
    end
  end

  def handle_event({:call, from}, :get_state, state, data) do
    {:next_state, state, data, [{:reply, from, data}]}
  end

  # Private functions

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

  @spec get_next_seat(list(), number()) :: number()
  defp get_next_seat(players, active_seat) do
    if active_seat + 1 == length(players) do
      1
    else
      active_seat + 1
    end
  end
end
