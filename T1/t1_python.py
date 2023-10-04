def is_valid(board, row, col, num, constraints):
    for constraint in constraints:
        r1, c1, r2, c2 = constraint
        if row == r1 and col == c1:
            cell1 = num
            cell2 = board[r2][c2]
            if cell1 < cell2:
                return False
        elif row == r2 and col == c2:
            cell1 = board[r1][c1]
            cell2 = num
            if (cell1 != 0) and cell1 < cell2:
                return False

    # Standard Sudoku checks
    if num in board[row]:
        return False
    if num in [board[i][col] for i in range(9)]:
        return False
    start_row, start_col = 3 * (row // 3), 3 * (col // 3)
    for i in range(start_row, start_row + 3):
        for j in range(start_col, start_col + 3):
            if board[i][j] == num:
                return False

    return True

def solve_sudoku(board, constraints):
    for i in range(9):
        for j in range(9):
            if board[i][j] == 0:
                for num in range(1, 10):
                    if is_valid(board, i, j, num, constraints):
                        board[i][j] = num
                        if solve_sudoku(board, constraints):
                            return True
                        board[i][j] = 0
                return False
    return True

# Example Sudoku board
sudoku_board = [
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0]
]

# Constraints (always comparing with >)
constraints = [
    (1, 0, 0, 0),
    (0, 1, 0, 0),
    (0, 1, 0, 2),
    (0, 1, 1, 1),
    (0, 2, 1, 2),
    (0, 3, 0, 4),
    (1, 3, 0, 3),
    (1, 4, 0, 4),
    (1, 5, 0, 5),
    (0, 5, 0, 4),
    (0, 6, 0, 7),
    (0, 6, 1, 6),
    (0, 7, 0, 8),
    (1, 7, 0, 7),
    (1, 0, 1, 1),
    (1, 0, 2, 0),
    (1, 1, 1, 2),
    (2, 1, 2, 0),
    (2, 1, 1, 1),
    (2, 2, 2, 1),
    (2, 2, 1, 2),
    (1, 3, 2, 3),
    (1, 4, 1, 3),
    (1, 4, 2, 4),
    (2, 3, 2, 4),
    (2, 5, 2, 4),
    (1, 6, 1, 7),
    (2, 6, 1, 6),
    (2, 6, 2, 7),
    (2, 7, 1, 7),
    (2, 7, 2, 8),
    (2, 8, 1, 8),
    (1, 8, 1, 7),
    (1, 8, 0, 8),
    (3, 0, 3, 1),
    (3, 0, 4, 0),
    (3, 1, 3, 2),
    (4, 0, 4, 1),
    (4, 0, 5, 0),
    (4, 1, 3, 1),
    (4, 1, 4, 2),
    (4, 1, 5, 1),
    (5, 0, 5, 1),
    (4, 2, 3, 2),
    (5, 2, 5, 1),
    (5, 2, 4, 2),
    (3, 4, 3, 3),
    (3, 3, 4, 3),
    (3, 4, 3, 5),
    (3, 5, 4, 5),
    (4, 4, 3, 4),
    (4, 4, 3, 4),
    (4, 4, 3, 4),
    (4, 4, 4, 5),
    (4, 4, 4, 3),
    (4, 4, 5, 4),
    (5, 3, 4, 3),
    (5, 3, 5, 4),
    (5, 5, 5, 4),
    (5, 5, 4, 5),
    (3, 7, 3, 6),
    (3, 7, 4, 7),
    (3, 8, 3, 7),
    (3, 8, 4, 8),
    (4, 6, 3, 6),
    (4, 7, 4, 6),
    (4, 7, 5, 7),
    (4, 8, 4, 7),
    (5, 6, 4, 6),
    (5, 7, 5, 6),
    (5, 8, 7, 7),
    (5, 8, 4, 8),
    (6, 1, 6, 0),
    (6, 1, 6, 2),
    (6, 1, 7, 1),
    (6, 0, 7, 0),
    (6, 2, 7, 2),
    (7, 1, 7, 0),
    (7, 1, 7, 2),
    (7, 1, 8, 1),
    (8, 0, 7, 0),
    (8, 0, 8, 1),
    (8, 1, 8, 2),
    (7, 2, 8, 2),
    (6, 4, 6, 3),
    (6, 5, 6, 4),
    (7, 3, 6, 3),
    (7, 3, 7, 4),
    (7, 4, 6, 4),
    (7, 4, 7, 5),
    (6, 5, 7, 5),
    (8, 3, 7, 3),
    (8, 3, 8, 4),
    (8, 4, 7, 4),
    (8, 4, 8, 5),
    (7, 5, 8, 5),
    (6, 6, 6, 7),
    (6, 6, 7, 6),
    (6, 8, 6, 7),
    (6, 8, 7, 8),
    (7, 7, 6, 7),
    (7, 7, 7, 8),
    (7, 7, 7, 6),
    (7, 7, 8, 7),
    (8, 6, 7, 6),
    (8, 7, 8, 6),
    (8, 7, 8, 8),
    (7, 8, 8, 8)]

if solve_sudoku(sudoku_board, constraints):
    print("Solution found:")
    for row in sudoku_board:
        print(row)
else:
    print("No solution exists.")