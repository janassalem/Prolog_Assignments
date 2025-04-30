% Our 5x5 grid world:
% D = Drone starting position
% P = Package to deliver
% O = Obstacle
% _ (explicitly identifing it ) and '' = Empty space
%we can resixe the grid and/or change the location of the drone's starting point
grid([
    ['D','','P','','_'],
    ['','O','','O','_'],
    ['','','P','',''],
    ['','O','','O','_'],
    ['P','','','_','P']
%    ,['','O','','O','_']
]).

% Possible movement directions (up, down, left, right)
%We imagined it as a Matrix(or a graph) thta has (X,Y) points
direction(up, -1, 0).
direction(down, 1, 0).
direction(left, 0, -1).
direction(right, 0, 1).

% Main entry point -> finds and displays the best delivery route
% Find where D is in the grid
% We built the path in reverse order
start :-
    grid(Grid),
    find_drone_position(Grid, DronePos),
    search(Grid, [DronePos], [], [], BestPath, MaxDeliveries),
    reverse(BestPath, FinalPath),
    format('Maximum deliveries possible: '), write(MaxDeliveries), nl,
    format('Optimal path: '), write(FinalPath), nl,
    print_solution_steps(FinalPath, Grid, [DronePos]).

% Find the position of the drone (D) in the grid as we can change it to our liking
%the nth0 is  built in predicate kol mohemtha enha bt search by index fel list
%nth0(starting point, list ely hndwar fyha , el element ely hn get it mn el list)
% bl 3ameya keda awel nth0 ht2ol get  el row ely wa2feen fyh (starting mn 0) w eltanya bt2ol dwarly ba2a 3ala
% el 'D' gowa kol row
find_drone_position(Grid, (R, C)) :-
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
search(Grid, [Current|Rest], VisitedP, PathSoFar, BestPath, MaxP) :-
    % Check if current position has an undelivered package "P" and avoid duplicates
    %bl 3ameya yaanie if there is an undelivered package then deliver it and mark as visted
    %else Mark as delivered  No delivery here
    (   is_undelivered_package(Grid, Current),
        \+ member(Current, VisitedP),
        !,
        NewVisitedP = [Current|VisitedP]
    ;   NewVisitedP = VisitedP
    ),

    % Add current position to our path to track the movments
    NewPath = [Current|PathSoFar],

    % Find all valid next moves we haven't visited yet
    %hya btshof fel grid  el current positin ely ehna fyha da eh el best directiom
    %btgm3hum w t7thom fi list el children
    findall(Next, (
        move(Grid, Current, Next),
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
             search(Grid, [Child,Current|Rest], NewVisitedP, NewPath, Path, P)),
            Results),

        max_by_deliveries(Results, BestPath, MaxP)
    ).

% Check if a move to (R2,C2) is valid from (R,C)
%hyakhod el list btaatna 3ady w hyget el direction based on the facts given above w yshof lw
%hya fel bounds wala laa aw el position da obstacle wala laa
move(Grid, (R,C), (R2,C2)) :-
    direction(_, DR, DC),
    R2 is R + DR,
    C2 is C + DC,
    in_bounds(Grid, R2, C2),
    \+ is_obstacle(Grid, (R2,C2)).

% Check if a cell contains an obstacle
%searching for the 'O'
is_obstacle(Grid, (R,C)) :-
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
is_undelivered_package(Grid, (R,C)) :-
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
%Base case howa eno option wahed
%recursive case hya en fy kaza option w bncompare kol path w bnakhid ely fyh aktr deliverables
max_by_deliveries([(D1,P1)], P1, D1) :- !.
max_by_deliveries([(D1,P1)|T], BestP, BestD) :-
    max_by_deliveries(T, P2, D2),
    (D1 > D2, !, BestP = P1, BestD = D1 ; BestP = P2, BestD = D2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%BONUS%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
print_solution_steps([Pos], Grid, VisitedPath) :-
    format('Final position: '), write(Pos), nl,
    update_grid_with_path(Pos, Grid, VisitedPath, FinalGrid),
    print_grid(FinalGrid).

%recursive case en yb2a fy lesa positions
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
%Base case eno yb2a el list khlset fa hytl3 nfs el grid
mark_path(Grid, [], Grid).
%recursive case eno ydkhol Grid w el op yb2a Updated grid
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
