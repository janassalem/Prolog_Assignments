:- use_module(library(lists)).
:- use_module(library(heaps)).

% Main entry point that shows a menu to choose which problem to run
main :-
    clear_screen,
    write('==================================='), nl,
    write('|    Delivery Drone Routing      |'), nl,
    write('==================================='), nl,
    write('| 1. Problem "1" Uninformed Search |'), nl,
    write('| 2. Problem "2" Informed Search   |'), nl,
    write('| 0. Exit the program              |'), nl,
    write('==================================='), nl,
    write('Choose an option: '),
    read(Option),
    process_option(Option).

% Process the menu option
process_option(1) :- problem1_start, wait_for_enter, main.
process_option(2) :- problem2_menu, wait_for_enter, main.
process_option(0) :- write('Goodbye!'), nl.
process_option(_) :- write('Invalid option, try again.'), nl, wait_for_enter, main.

% Wait for user to press Enter
wait_for_enter :-
    nl, write('Press Enter to continue...'),
    get_single_char(_),
    nl.

% Clear the terminal screen
clear_screen :- write('\e[2J'), write('\e[H').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PROBLEM 1: Uninformed Search (DFS)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Our 5x5 grid world:
% D = Drone starting position
% P = Package to deliver
% O = Obstacle
% _ (explicitly identifing it ) and '' = Empty space
%we can resixe the grid and/or change the location of the drone's starting point
problem1_grid([
    ['D','','P','','_'],
    ['','O','','O','_'],
    ['','','P','',''],
    ['','O','','O','_'],
    ['P','','','_','P']
]).

% Possible movement directions (up, down, left, right)
%We imagined it as a Matrix(or a graph) thta has (X,Y) points
problem1_direction(up, -1, 0).
problem1_direction(down, 1, 0).
problem1_direction(left, 0, -1).
problem1_direction(right, 0, 1).

% Main entry point for Problem 1
% Main entry point -> finds and displays the best delivery route
% Find where D is in the grid
% We built the path in reverse order
problem1_start :-
    clear_screen,
    write('Problem 1: Drone Routing with Uninformed Search (DFS)'), nl, nl,
    problem1_grid(Grid),
    problem1_find_drone_position(Grid, DronePos),
    problem1_search(Grid, [DronePos], [], [], BestPath, MaxDeliveries),
    reverse(BestPath, FinalPath),
    format('Maximum deliveries possible: '), write(MaxDeliveries), nl,
    format('Optimal path: '), write(FinalPath), nl,
    problem1_print_solution_steps(FinalPath, Grid, [DronePos]).

% Find the position of the drone (D) in the grid as we can change it to our liking
%the nth0 is  built in predicate kol mohemtha enha bt search by index fel list
%nth0(starting point, list ely hndwar fyha , el element ely hn get it mn el list)
% bl 3ameya keda awel nth0 ht2ol get  el row ely wa2feen fyh (starting mn 0) w eltanya bt2ol dwarly ba2a 3ala
% el 'D' gowa kol row
problem1_find_drone_position(Grid, (R, C)) :-
    nth0(R, Grid, Row),
    nth0(C, Row, 'D').

/*
 * Depth-First Search algorithm to explore all possible paths
 * Parameters:
 *   Grid - The world map
 *   [Current|Rest] - Current position and rest of path to explore
 *   VisitedP - List of already delivered packages
 *   PathSoFar - Accumulated path
 *   BestPath - Output variable for best path found
 *   MaxP - Output variable for maximum deliveries count
 */
problem1_search(Grid, [Current|Rest], VisitedP, PathSoFar, BestPath, MaxP) :-
    % Check if current position has an undelivered package "P" and avoid duplicates
       %bl 3ameya yaanie if there is an undelivered package then deliver it and mark as visted
       %else Mark as delivered  No delivery here
    (   problem1_is_undelivered_package(Grid, Current),
        \+ member(Current, VisitedP),
        !,
        NewVisitedP = [Current|VisitedP]
    ;   NewVisitedP = VisitedP
    ),

    % Add current position to our path
    NewPath = [Current|PathSoFar],

      % Find all valid next moves we haven't visited yet
        %hya btshof fel grid  el current positin ely ehna fyha da eh el best directiom
        %btgm3hum w t7thom fi list el children
    findall(Next, (
        problem1_move(Grid, Current, Next),
        \+ member(Next, NewPath)
    ), Children),

     % Base case: no more moves (path ends)
        % Count deliveries w returns el path
    (   Children = [],
        !,
        length(NewVisitedP, Count),
        BestPath = NewPath,
        MaxP = Count
      % Recursive case: explore all children
    %    Current Position
    %    ├─ Move 1 → (recursive search)
    %    ├─ Move 2 → (recursive search)
    %    └─ Move 3 → (recursive search)
     % then Choose the path with most deliveries
    ;   findall((P,Path),
            (member(Child, Children),
             problem1_search(Grid, [Child,Current|Rest], NewVisitedP, NewPath, Path, P)),
            Results),

        problem1_max_by_deliveries(Results, BestPath, MaxP)
    ).

% Check if a move to (R2,C2) is valid from (R,C)
%hyakhod el list btaatna 3ady w hyget el direction based on the facts given above w yshof lw
%hya fel bounds wala laa aw el position da obstacle wala laa
problem1_move(Grid, (R,C), (R2,C2)) :-
    problem1_direction(_, DR, DC),
    R2 is R + DR,
    C2 is C + DC,
    problem1_in_bounds(Grid, R2, C2),
    \+ problem1_is_obstacle(Grid, (R2,C2)).

% Check if a cell contains an obstacle
%searching for the 'O'
problem1_is_obstacle(Grid, (R,C)) :-
    nth0(R, Grid, Row),
    nth0(C, Row, 'O').

% Check if a cell contains a package to deliver
%searching for 'P'
%Grid:
%Row 0: [ , ,P, , ]
%Row 1: [ ,O, ,O, ]
%Row 2: [ , ,D, , ]  ← Drone here
%Row 3: [ ,O, ,O, ]
%Row 4: [P, , , ,P]
%
%Execution Steps:
%1. Tries UP to (1,2) → finds obstacle 'O' → fails
%2. Tries DOWN to (3,2) → finds obstacle 'O' → fails
%3. Tries LEFT to (2,1) → empty space → SUCCESS
%   • Returns (2,1) as valid move
%4. If caller continues backtracking, would try RIGHT to (2,3)
problem1_is_undelivered_package(Grid, (R,C)) :-
    nth0(R, Grid, Row),
    nth0(C, Row, 'P').

% Verify coordinates are within grid boundaries
problem1_in_bounds(Grid, R, C) :-
    length(Grid, Rows),
    nth0(0, Grid, Row),
    length(Row, Cols),
    R >= 0, R < Rows,
    C >= 0, C < Cols.


% Select the path with maximum deliveries
%Base case howa eno option wahed
%recursive case hya en fy kaza option w bncompare kol path w bnakhid ely fyh aktr deliverables
problem1_max_by_deliveries([(D1,P1)], P1, D1) :- !.
problem1_max_by_deliveries([(D1,P1)|T], BestP, BestD) :-
    problem1_max_by_deliveries(T, P2, D2),
    (D1 > D2, !, BestP = P1, BestD = D1 ; BestP = P2, BestD = D2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%P1-BONUS%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper to print the solution steps with path marked with *
%[(0,0), (0,1), (1,1)]
%Current position: (0,0)
%[ D * _ _ _ ]
%[ _ _ _ _ _ ]
%...
%
%Current position: (0,1)
%[ * D _ _ _ ]
%[ _ _ _ _ _ ]
%...
%
%Final position: (1,1)
%[ * * _ _ _ ]
%[ _ D _ _ _ ]
%...
%Base case en da yb2a el final position only one position remains in pos
problem1_print_solution_steps([Pos], Grid, VisitedPath) :-
    format('Final position: '), write(Pos), nl,
    problem1_update_grid_with_path(Pos, Grid, VisitedPath, FinalGrid),
    problem1_print_grid(FinalGrid).
%recursive case en yb2a fy lesa positions
problem1_print_solution_steps([Pos|Rest], Grid, VisitedPath) :-
    format('Current position: '), write(Pos), nl,
    problem1_update_grid_with_path(Pos, Grid, VisitedPath, UpdatedGrid),
    problem1_print_grid(UpdatedGrid),
    nl,
    problem1_print_solution_steps(Rest, UpdatedGrid, [Pos|VisitedPath]).

% Update grid showing current position (D) and path (*)
problem1_update_grid_with_path(CurrentPos, Grid, VisitedPath, UpdatedGrid) :-
    problem1_mark_path(Grid, VisitedPath, TempGrid),
    problem1_replace_in_grid(CurrentPos, 'D', TempGrid, UpdatedGrid).

% Mark all visited positions with * except current position
%Base case eno yb2a el list khlset fa hytl3 nfs el grid
problem1_mark_path(Grid, [], Grid).
problem1_mark_path(Grid, [Pos|Rest], MarkedGrid) :-
    problem1_replace_in_grid(Pos, '*', Grid, TempGrid),
    problem1_mark_path(TempGrid, Rest, MarkedGrid).

% Replace an element in the grid
problem1_replace_in_grid((X,Y), Value, Grid, UpdatedGrid) :-
    problem1_replace_in_grid_rows(X, Y, Value, Grid, UpdatedGrid).

problem1_replace_in_grid_rows(0, Y, Value, [Row|Rows], [UpdatedRow|Rows]) :-
    problem1_replace_in_row(Y, Value, Row, UpdatedRow).
problem1_replace_in_grid_rows(X, Y, Value, [Row|Rows], [Row|UpdatedRows]) :-
    X > 0,
    X1 is X - 1,
    problem1_replace_in_grid_rows(X1, Y, Value, Rows, UpdatedRows).

problem1_replace_in_row(0, Value, [_|Tail], [Value|Tail]).
problem1_replace_in_row(Y, Value, [Head|Tail], [Head|UpdatedTail]) :-
    Y > 0,
    Y1 is Y - 1,
    problem1_replace_in_row(Y1, Value, Tail, UpdatedTail).

% Print the grid
problem1_print_grid([]).
problem1_print_grid([Row|Rows]) :-
    problem1_print_row(Row),
    nl,
    problem1_print_grid(Rows).

problem1_print_row([]).
problem1_print_row([Cell|Cells]) :-
    (Cell == '', !, write('_') ; write(Cell)),
    write(' '),
    problem1_print_row(Cells).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PROBLEM 2: Informed Search (A*)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Problem 2 grid configuration
problem2_grid_size(5, 5).

% Define obstacles in the grid for Problem 2
problem2_obstacle((1, 2)).
problem2_obstacle((1, 4)).
problem2_obstacle((3, 2)).
problem2_obstacle((3, 4)).
problem2_obstacle((4, 3)).

% Define delivery points for Problem 2
problem2_delivery_point((3, 1)).
problem2_delivery_point((1, 3)).
problem2_delivery_point((5, 5)).

% Define recharge station for Problem 2
problem2_recharge_station((2, 4)).

% Starting position for Problem 2
problem2_start_position((1, 1)).

% Define valid moves for Problem 2
problem2_valid_move((X, Y), (NX, Y)) :-
    NX is X + 1,
    problem2_grid_size(W, _),
    NX =< W,
    \+ problem2_obstacle((NX, Y)).
problem2_valid_move((X, Y), (NX, Y)) :-
    NX is X - 1,
    NX > 0,
    \+ problem2_obstacle((NX, Y)).
problem2_valid_move((X, Y), (X, NY)) :-
    NY is Y + 1,
    problem2_grid_size(_, H),
    NY =< H,
    \+ problem2_obstacle((X, NY)).
problem2_valid_move((X, Y), (X, NY)) :-
    NY is Y - 1,
    NY > 0,
    \+ problem2_obstacle((X, NY)).

% Manhattan distance heuristic for A*
problem2_manhattan_distance((X1, Y1), (X2, Y2), D) :-
    D is abs(X1 - X2) + abs(Y1 - Y2).

% Calculate heuristic to nearest delivery point
problem2_heuristic(_, [], 0) :- !.
problem2_heuristic(Position, DeliveryPoints, H) :-
    findall(Distance,
            (member(DeliveryPoint, DeliveryPoints),
             problem2_manhattan_distance(Position, DeliveryPoint, Distance)),
            Distances),
    min_list(Distances, H).

% Update energy based on the current position
problem2_update_energy(CurrentEnergy, Position, NewEnergy) :-
    (problem2_recharge_station(Position) ->
        % Recharge to full if at recharge station
        NewEnergy = 6
    ;
        % Otherwise decrease energy by 1
        NewEnergy is CurrentEnergy - 1
    ),
    % Ensure energy is positive
    NewEnergy > 0.

% Update the list of remaining delivery points
problem2_update_deliveries(Position, CurrentDeliveries, NewDeliveries) :-
    (select(Position, CurrentDeliveries, NewDeliveries) -> true ; NewDeliveries = CurrentDeliveries).

% A* search implementation with energy constraints
problem2_astar_search(UseEnergy, MaxEnergy, Path, FinalEnergy) :-
    problem2_start_position(Start),
    findall(P, problem2_delivery_point(P), DeliveryPoints),
    empty_heap(EmptyHeap),
    problem2_heuristic(Start, DeliveryPoints, H),
    add_to_heap(EmptyHeap, H, state(Start, [Start], MaxEnergy, DeliveryPoints), InitialHeap),
    problem2_astar_loop(UseEnergy, InitialHeap, Path, FinalEnergy).

% Base case: All deliveries completed
problem2_astar_loop(_, Heap, Path, RemainingEnergy) :-
    get_from_heap(Heap, _, state(Position, ReversePath, RemainingEnergy, []), _),
    reverse(ReversePath, Path).

% Recursive case: Continue search
problem2_astar_loop(UseEnergy, Heap, Path, FinalEnergy) :-
    get_from_heap(Heap, _, state(Position, ReversePath, Energy, RemainingDeliveries), RestHeap),
    RemainingDeliveries \= [],
    findall(NewState,
            (problem2_valid_move(Position, NextPosition),
             \+ member(NextPosition, ReversePath),  % Avoid cycles
             (UseEnergy ->
                problem2_update_energy(Energy, NextPosition, NewEnergy)
             ;
                NewEnergy = Energy  % Ignore energy if not using constraints
             ),
             problem2_update_deliveries(NextPosition, RemainingDeliveries, NewDeliveries),
             NewState = state(NextPosition, [NextPosition|ReversePath], NewEnergy, NewDeliveries)),
            NextStates),
    problem2_add_states_to_heap(NextStates, RestHeap, NewHeap),
    problem2_astar_loop(UseEnergy, NewHeap, Path, FinalEnergy).

% Add multiple states to the heap
problem2_add_states_to_heap([], Heap, Heap).
problem2_add_states_to_heap([state(Position, ReversePath, Energy, Deliveries)|Rest], Heap, NewHeap) :-
    length(ReversePath, G),  % Cost so far is path length
    problem2_heuristic(Position, Deliveries, H),  % Heuristic to nearest delivery
    F is G + H,  % F = G + H (A* formula)
    add_to_heap(Heap, F, state(Position, ReversePath, Energy, Deliveries), TempHeap),
    problem2_add_states_to_heap(Rest, TempHeap, NewHeap).

% Solve drone delivery problem
problem2_solve_drone_delivery(UseEnergy, InitialEnergy, Path, RemainingEnergy) :-
    problem2_astar_search(UseEnergy, InitialEnergy, Path, RemainingEnergy).

% Print the Problem 2 grid
problem2_print_grid(CurrentPosition, Path) :-
    problem2_grid_size(Width, Height),
    write('Grid representation:'), nl,
    forall(between(1, Height, Y), (
        forall(between(1, Width, X), (
            problem2_print_cell((X, Y), CurrentPosition, Path)
        )),
        nl
    )).

% Print a single cell in the grid
problem2_print_cell(Position, CurrentPosition, Path) :-
    (Position = CurrentPosition -> write('D ');
     member(Position, Path), Position \= CurrentPosition -> write('* ');
     problem2_obstacle(Position) -> write('O ');
     problem2_delivery_point(Position) -> write('P ');
     problem2_recharge_station(Position) -> write('R ');
     write('- ')).

% Display the solution with detailed steps
problem2_display_solution(UseEnergy, InitialEnergy) :-
    format('Solving with initial energy: ~w~n~n', [InitialEnergy]),

    % Try to find a solution
    (problem2_solve_drone_delivery(UseEnergy, InitialEnergy, Path, RemainingEnergy) ->
        format('Solution found!~n'),
        format('Path: ~w~n', [Path]),
        format('Path length: ~w steps~n', [Path]),
        format('Remaining energy: ~w~n~n', [RemainingEnergy]),

        % Print initial and final states
        format('Initial state:~n'),
        problem2_start_position(Start),
        problem2_print_grid(Start, []),
        nl,

        % Print final state
        format('Final state:~n'),
        last(Path, FinalPosition),
        problem2_print_grid(FinalPosition, Path),
        nl,

        % Print key points in the journey
        problem2_display_key_points(Path, InitialEnergy)
    ;
        format('No solution found! The drone cannot complete all deliveries with ~w energy.~n', [InitialEnergy])
    ).

% Display key points in the journey (delivery points and recharge)
problem2_display_key_points([_], _) :- !.
problem2_display_key_points([Current, Next|Rest], Energy) :-
    (problem2_delivery_point(Next) ->
        format('Delivery at position ~w~n', [Next]),
        problem2_print_grid(Next, [Current, Next|Rest]),
        nl,
        problem2_calculate_energy(Current, Next, Energy, NewEnergy)
    ;
     problem2_recharge_station(Next) ->
        format('Recharging at position ~w~n', [Next]),
        problem2_print_grid(Next, [Current, Next|Rest]),
        nl,
        NewEnergy = 6
    ;
        problem2_calculate_energy(Current, Next, Energy, NewEnergy)
    ),
    problem2_display_key_points([Next|Rest], NewEnergy).

% Calculate energy after a move
problem2_calculate_energy(_, Position, Energy, NewEnergy) :-
    (problem2_recharge_station(Position) -> NewEnergy = 6 ; NewEnergy is Energy - 1).

% Menu for Problem 2
problem2_menu :-
    clear_screen,
    write('Problem 2: Drone Routing with Informed Search (A*)'), nl, nl,
    write('Run Problem 2 with energy constraints?'), nl,
    write('1. Standard A* (no energy constraints)'), nl,
    write('2. A* with energy constraints'), nl,
    write('Choose option: '),
    read(Option),
    (Option = 1 ->
        problem2_run(false)
    ; Option = 2 ->
        problem2_run(true)
    ;
        write('Invalid option'), nl,
        problem2_menu
    ).

% Run Problem 2 with or without energy constraints
problem2_run(false) :-
    problem2_display_solution(false, 999).  % Large energy for unconstrained version
problem2_run(true) :-
    write('Enter initial energy for drone (e.g., 9): '),
    read(Energy),
    problem2_display_solution(true, Energy).

% Direct function to run drone delivery with specific energy
run_drone_delivery(Energy) :-
    problem2_display_solution(true, Energy).

% Start main menu automatically on loading
:- initialization(main).