% Include the league data
:-consult(league_data).

% TASK 1: Get a list of all players in a specific team
players_in_team(Team, PlayersList) :-
    collect_all_players(Team, [], PlayersList),
    !.

collect_all_players(Team, Acc, Result) :- % Finds players associated with given team and accumulates them.
    player(Player, Team, _),
    \+ member(Player, Acc), % make sure that there are no duplicates
    !,
    collect_all_players(Team, [Player|Acc], Result).% Recursive call -> updated list
collect_all_players(_, Result, Result).% Base case

% TASK 2: Count how many teams are from a specific country
team_count_by_country(Country, Count) :-
    count_country_teams(Country, Count),
    !.

count_country_teams(Country, Count) :-
    collect_country_teams(Country, [], Teams),
    length(Teams, Count).

collect_country_teams(Country, Acc, Result) :-
    team(Team, Country, _),
    \+ member(Team, Acc),
    !,
    collect_country_teams(Country, [Team|Acc], Result).
collect_country_teams(_, Result, Result). % Base case

% TASK 3: Find the team with the most championship titles
most_successful_team(Team) :-
    find_max_titles_team(Team),
    !.

find_max_titles_team(Team) :-
    team(Team, _, Titles),
    \+ (team(OtherTeam, _, OtherTitles), OtherTeam \= Team, OtherTitles > Titles). %sees if there is a team with more titles

% TASK 4: List all matches where a specific team participated
matches_of_team(Team, Matches) :-
    collect_team_matches(Team, Matches),
    !.

collect_team_matches(Team, AllMatches) :-
    collect_team_home_matches(Team, [], HomeMatches),
    collect_team_away_matches(Team, HomeMatches, AllMatches).

collect_team_home_matches(Team, Acc, Result) :-
    match(Team, Opponent, GoalsFor, GoalsAgainst),
    \+ member((Team, Opponent, GoalsFor, GoalsAgainst), Acc),
    !,
    collect_team_home_matches(Team, [(Team, Opponent, GoalsFor, GoalsAgainst)|Acc], Result).
collect_team_home_matches(_, Result, Result).

collect_team_away_matches(Team, Acc, Result) :-
    match(Opponent, Team, GoalsAgainst, GoalsFor),
    \+ member((Opponent, Team, GoalsAgainst, GoalsFor), Acc),
    !,
    collect_team_away_matches(Team, [(Opponent, Team, GoalsAgainst, GoalsFor)|Acc], Result).
collect_team_away_matches(_, Result, Result).

% TASK 5: Count all matches where a specific team participated
num_matches_of_team(Team, Count) :-
    count_team_matches(Team, Count),
    !.

count_team_matches(Team, Count) :-
    matches_of_team(Team, Matches),
    length(Matches, Count).

% TASK 6: Find the top goal scorer in the tournament
top_scorer(Player) :-
    find_highest_scorer(Player),
    !.

find_highest_scorer(Player) :-
    goals(Player, Goals),
    \+ (goals(OtherPlayer, OtherGoals), OtherPlayer \= Player, OtherGoals > Goals).

% TASK 7: Find the Most Common Position in a Specific Team
most_common_position_in_team(Team, Position) :-
    find_most_common_position(Team, Position),
    !.

find_most_common_position(Team, MostCommonPos) :-
    collect_team_positions(Team, [], Positions),
    count_positions_occurrences(Team, Positions, [], PositionCounts),
    find_max_position(PositionCounts, 0, '', MostCommonPos).

collect_team_positions(Team, Acc, Result) :-
    player(_, Team, Position),
    \+ member(Position, Acc),
    !,
    collect_team_positions(Team, [Position|Acc], Result).
collect_team_positions(_, Result, Result).

count_positions_occurrences(_, [], Result, Result).
count_positions_occurrences(Team, [Position|Positions], Acc, Result) :-
    count_position_players(Team, Position, 0, Count),
    count_positions_occurrences(Team, Positions, [(Position, Count)|Acc], Result).

count_position_players(Team, Position, Count, Result) :-
    player(_, Team, Position),
    \+ counted_player(Team, Position),
    assert(counted_player(Team, Position)),
    NewCount is Count + 1,
    !,
    count_position_players(Team, Position, NewCount, Result).
count_position_players(Team, Position, Result, Result) :-
    retractall(counted_player(Team, Position)).

find_max_position([], _, MaxPos, MaxPos).
find_max_position([(Position, Count)|Rest], MaxCount, _, MaxPos) :-
    Count > MaxCount,
    !,
    find_max_position(Rest, Count, Position, MaxPos).
find_max_position([(_, Count)|Rest], MaxCount, CurrMaxPos, MaxPos) :-
    Count =< MaxCount,
    !,
    find_max_position(Rest, MaxCount, CurrMaxPos, MaxPos).

% Helper predicates
:- dynamic counted_player/2.