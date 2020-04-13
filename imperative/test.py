import numpy as np
import multiprocessing
import time
import unittest

from solver import gauss


class TestGaussSolver(unittest.TestCase):
    def test_example_from_wikipedia(self):
        # https://en.wikipedia.org/wiki/System_of_linear_equations
        A = [[3, 2, -1], [2, -2, 4], [-1, 0.5, -1]]
        b = [1, -2, 0]
        X = [1, -2, -2]
        X_found = gauss(A, b, 1)

        np.testing.assert_allclose(X, X_found)

    def test_another_example_from_guide(self):
        # https://martin-thoma.com/solving-linear-equations-with-gaussian-elimination/
        A = [[1.0, 2.0, 3.0], [4.0, 5.0, 6.0], [1.0, 0.0, 1.0]]
        b = [1.0, 1.0, 1.0]
        X = [0, -1, 1]
        X_found = gauss(A, b, 1)

        np.testing.assert_allclose(X, X_found)

    def test_big_matrix(self):
        A = np.random.random((10, 10)) * 1000
        X = np.arange(10)
        b = np.dot(A, X.T)
        X_found = gauss(A, b, 1)

        np.testing.assert_allclose(X, X_found)

    def test_parallelism(self):
        return
        N = 10
        A = np.random.random((N, N)) * 1000
        X = np.arange(N)
        b = np.dot(A, X)

        for p in range(1, multiprocessing.cpu_count()):
            t_start = time.time()
            X_i = gauss(A, b, p)
            print(f"Num of proc: {p}, time: {time.time() - t_start}")


if __name__ == "__main__":
    unittest.main()
