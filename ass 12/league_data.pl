% Data in league_data.pl

% Team(Name, Country, Num_of_winning_times)
team(barcelona, spain, 5).
team(bayern_munich, germany, 6).
team(liverpool, england, 3).
team(real_madrid, spain, 13).

% Player(Name, Team, Position)
player(messi, barcelona, forward).
player(ter_stegen, barcelona, goalkeeper).
player(de_jong, barcelona, midfielder).
player(ronaldo, real_madrid, forward).
player(modric, real_madrid, midfielder).

% Match(Team1, Team2, Team1Goals, Team2Goals)
match(barcelona, real_madrid, 3, 2).
match(liverpool, barcelona, 2, 2).
match(bayern_munich, real_madrid, 4, 1).

% Goals(Player, Num_of_scored_goals)
goals(messi, 10).
goals(ronaldo, 12).
goals(modric, 5).
