/*
 * Drone Delivery Route Planner using DFS
 *
 * This program finds the optimal path for a drone to deliver packages (P)
 * while avoiding obstacles (O) in a grid world.
 * The drone starts at (0,0) and aims to maximize deliveries.
 */

% Our 5x5 grid world:
% D = Drone starting position
% P = Package to deliver
% O = Obstacle
% _ = Empty space
grid([
    ['D','','P','','_'],
    ['','O','','O','_'],
    ['','','P','',''],
    ['','O','','O','_'],
    ['P','','','_','P']
]).

% Possible movement directions (up, down, left, right)
direction(up, -1, 0).
direction(down, 1, 0).
direction(left, 0, -1).
direction(right, 0, 1).

% Main entry point - finds and displays the best delivery route
start :-
    grid(Grid),
    search(Grid, [(0,0)], [], [], BestPath, MaxDeliveries),
    reverse(BestPath, FinalPath), % We built the path in reverse order
    format('Maximum deliveries possible: '), write(MaxDeliveries), nl,
    format('Optimal path: '), write(FinalPath), nl,
    print_solution_steps(FinalPath, Grid, [(0,0)]).

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
search(Grid, [Current|Rest], VisitedP, PathSoFar, BestPath, MaxP) :-
    % Check if current position has an undelivered package
    (   is_delivery(Grid, Current),
        \+ member(Current, VisitedP),
        !,
        NewVisitedP = [Current|VisitedP] % Mark as delivered
    ;   NewVisitedP = VisitedP           % No delivery here
    ),

    % Add current position to our path
    NewPath = [Current|PathSoFar],

    % Find all valid next moves we haven't visited yet
    findall(Next, (
        move(Grid, Current, Next),
        \+ member(Next, NewPath)
    ), Children),

    % Base case: no more moves (path ends)
    (   Children = [],
        !,
        length(NewVisitedP, Count), % Count deliveries
        BestPath = NewPath,
        MaxP = Count
    % Recursive case: explore all children
    ;   findall((P,Path),
            (member(Child, Children),
             search(Grid, [Child,Current|Rest], NewVisitedP, NewPath, Path, P)),
            Results),
        % Choose the path with most deliveries
        max_by_deliveries(Results, BestPath, MaxP)
    ).

% Check if a move to (R2,C2) is valid from (R,C)
move(Grid, (R,C), (R2,C2)) :-
    direction(_, DR, DC),     % Get any direction
    R2 is R + DR,            % Calculate new row
    C2 is C + DC,            % Calculate new column
    in_bounds(Grid, R2, C2), % Check within grid
    \+ is_obstacle(Grid, (R2,C2)). % Check not an obstacle

% Check if a cell contains an obstacle
is_obstacle(Grid, (R,C)) :-
    nth0(R, Grid, Row),
    nth0(C, Row, 'O').

% Check if a cell contains a package to deliver
is_delivery(Grid, (R,C)) :-
    nth0(R, Grid, Row),
    nth0(C, Row, 'P').

% Verify coordinates are within grid boundaries
in_bounds(Grid, R, C) :-
    length(Grid, Rows),
    nth0(0, Grid, Row),
    length(Row, Cols),
    R >= 0, R < Rows,
    C >= 0, C < Cols.

% Select the path with maximum deliveries
max_by_deliveries([(D1,P1)], P1, D1) :- !.
max_by_deliveries([(D1,P1)|T], BestP, BestD) :-
    max_by_deliveries(T, P2, D2),
    (D1 > D2, !, BestP = P1, BestD = D1 ; BestP = P2, BestD = D2).

% Helper to print the solution steps with path marked with *
print_solution_steps([Pos], Grid, VisitedPath) :-
    format('Final position: '), write(Pos), nl,
    update_grid_with_path(Pos, Grid, VisitedPath, FinalGrid),
    print_grid(FinalGrid).
print_solution_steps([Pos|Rest], Grid, VisitedPath) :-
    format('Current position: '), write(Pos), nl,
    update_grid_with_path(Pos, Grid, VisitedPath, UpdatedGrid),
    print_grid(UpdatedGrid),
    nl,
    print_solution_steps(Rest, UpdatedGrid, [Pos|VisitedPath]).

% Update grid showing current position (D) and path (*)
update_grid_with_path(CurrentPos, Grid, VisitedPath, UpdatedGrid) :-
    mark_path(Grid, VisitedPath, TempGrid),
    replace_in_grid(CurrentPos, 'D', TempGrid, UpdatedGrid).

% Mark all visited positions with * except current position
mark_path(Grid, [], Grid).
mark_path(Grid, [Pos|Rest], MarkedGrid) :-
    replace_in_grid(Pos, '*', Grid, TempGrid),
    mark_path(TempGrid, Rest, MarkedGrid).

% Helper to replace an element in the grid at (X,Y)
replace_in_grid((X,Y), Value, Grid, UpdatedGrid) :-
    replace_in_grid_rows(X, Y, Value, Grid, UpdatedGrid).

% Helper to replace an element in the grid rows
replace_in_grid_rows(0, Y, Value, [Row|Rows], [UpdatedRow|Rows]) :-
    replace_in_row(Y, Value, Row, UpdatedRow).
replace_in_grid_rows(X, Y, Value, [Row|Rows], [Row|UpdatedRows]) :-
    X > 0,
    X1 is X - 1,
    replace_in_grid_rows(X1, Y, Value, Rows, UpdatedRows).

% Helper to replace an element in a row
replace_in_row(0, Value, [_|Tail], [Value|Tail]).
replace_in_row(Y, Value, [Head|Tail], [Head|UpdatedTail]) :-
    Y > 0,
    Y1 is Y - 1,
    replace_in_row(Y1, Value, Tail, UpdatedTail).

% Helper to print the grid
print_grid([]).
print_grid([Row|Rows]) :-
    print_row(Row),
    nl,
    print_grid(Rows).

print_row([]).
print_row([Cell|Cells]) :-
    (Cell == '', !, write('_') ; write(Cell)),
    write(' '),
    print_row(Cells).