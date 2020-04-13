import sys
import operator

from multiprocessing import Pool, Array


argmax = lambda l: max(enumerate(l), key=operator.itemgetter(1))[0]


def get_matrix_element(matrix_array, i, j, num_rows):
    num_columns = len(matrix_array) // num_rows

    return matrix_array[i * num_columns + j]


def set_matrix_element(matrix_array, i, j, value, num_rows):
    num_columns = len(matrix_array) // num_rows
    matrix_array[i * num_columns + j] = value


def matrix_column(matrix_array, column_index, num_rows):
    return [
        get_matrix_element(matrix_array, i, column_index, num_rows)
        for i in range(num_rows)
    ]


def swap_row_inplace(matrix_array, i, j, num_rows):
    num_columns = len(matrix_array) // num_rows
    (
        matrix_array[i * num_columns : (i + 1) * num_columns],
        matrix_array[j * num_columns : (j + 1) * num_columns],
    ) = (
        matrix_array[j * num_columns : (j + 1) * num_columns],
        matrix_array[i * num_columns : (i + 1) * num_columns],
    )


def pprint_matrix(matrix_array, num_rows):
    num_columns = len(matrix_array) // num_rows
    for i in range(num_rows):
        for j in range(num_columns):
            print(get_matrix_element(matrix_array, i, j, num_rows), end="\t")
        print()
    print()


def partial_trinagulation(args):
    global matrix
    j, i, num_rows = args
    num_columns = len(matrix) // num_rows

    c = -get_matrix_element(matrix, j, i, num_rows) / get_matrix_element(
        matrix, i, i, num_rows
    )
    for k in range(i, num_columns):
        set_matrix_element(
            matrix,
            j,
            k,
            get_matrix_element(matrix, j, k, num_rows)
            + c * get_matrix_element(matrix, i, k, num_rows),
            num_rows,
        )


def init_globals(shared_matrix):
    global matrix
    matrix = shared_matrix


def gauss(A, b, num_of_threads=1):
    num_of_equations = len(A)
    num_of_unknowns = len(A[1])
    if num_of_equations != num_of_unknowns:
        raise NotImplementedError

    shared_matrix = Array(
        "d", sum([[*AI, bI] for AI, bI in zip(A, b)], []), lock=False
    )

    for i in range(num_of_equations - 1):
        i_column = matrix_column(shared_matrix, i, num_of_equations)
        max_row_index = argmax(i_column[i + 1 :]) + i
        swap_row_inplace(shared_matrix, max_row_index, i, num_of_equations)

        with Pool(initializer=init_globals, initargs=(shared_matrix,)) as pool:
            pool.map(
                partial_trinagulation,
                [
                    (j, i, num_of_equations)
                    for j in range(i + 1, num_of_equations)
                ],
            )

    x = [0 for i in range(num_of_equations)]
    for i in range(num_of_equations - 1, -1, -1):
        x[i] = get_matrix_element(
            shared_matrix, i, num_of_equations, num_of_equations
        ) / get_matrix_element(shared_matrix, i, i, num_of_equations)
        for k in range(i - 1, -1, -1):
            set_matrix_element(
                shared_matrix,
                k,
                num_of_equations,
                get_matrix_element(
                    shared_matrix, k, num_of_equations, num_of_equations
                )
                - get_matrix_element(shared_matrix, k, i, num_of_equations)
                * x[i],
                num_of_equations,
            )
            set_matrix_element(shared_matrix, k, i, 0, num_of_equations)
    return x


def main():
    _, p, _, _, A_path, b_path, *_ = sys.argv

    A = [
        [float(coefficient) for coefficient in row.strip().split()]
        for row in open(A_path).readlines()
    ]
    b = [float(constant) for constant in open(b_path).read().strip().split()]

    X = gauss(A, b, num_of_threads=int(p))

    with open("solution.txt", "w") as f:
        print(*X, file=f)


if __name__ == "__main__":
    main()
