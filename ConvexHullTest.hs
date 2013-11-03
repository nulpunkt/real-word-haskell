import Test.HUnit
import ConvexHull

testFindStart = TestCase (do
		assertEqual "" (7,1) (findStart [(5,9), (3,7), (10, 1), (7,1), (2,2), (3,2)])
	)

testOrderByPolarAngel = TestCase (do
		assertEqual "" [(7,1), (9,4), (7,6), (4,3)] (orderByPolarAngel (2,1) [(9,4), (7,1), (7,6), (4,3)])
	)

testCCW = TestCase (do
		assertEqual "" True (ccw (2,1) (7,3) (4,3))
	)

testFilterPoints = TestCase (do
		assertEqual "" [(2,1), (7,3), (3,6), (1,4)] (filterPoints [(2,1), (7,3), (4,3), (3,6), (1,4)])
	)

testFindHull = TestCase (do
		let input = [(2,1), (4,3), (7,1), (7,3), (9,4), (7,6), (3,6), (1,4)]
		let expected = [(7,1), (9,4), (7,6), (3,6), (1,4), (2,1)]
		assertEqual "" expected (hull input)
	)

tests = TestList [
					TestLabel "testFindStart" testFindStart,
					TestLabel "testOrderByPolarAngel" testOrderByPolarAngel,
					TestLabel "testCCW" testCCW,
					TestLabel "testFilterPoints" testFilterPoints,
					TestLabel "testFindHull" testFindHull
				]
main = runTestTT tests
