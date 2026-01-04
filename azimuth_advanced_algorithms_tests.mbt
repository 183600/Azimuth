// Azimuth Telemetry System - Advanced Algorithms Tests
// This file contains test cases for advanced algorithms used in the telemetry system

// Test 1: Sorting Algorithms
test "sorting_algorithms" {
  let unsorted_array = [64, 34, 25, 12, 22, 11, 90, 88, 45, 50]
  let expected_sorted = [11, 12, 22, 25, 34, 45, 50, 64, 88, 90]
  
  // Test bubble sort
  let bubble_sorted = SortUtil::bubble_sort(unsorted_array)
  assert_eq(bubble_sorted, expected_sorted)
  
  // Test quick sort
  let quick_sorted = SortUtil::quick_sort(unsorted_array)
  assert_eq(quick_sorted, expected_sorted)
  
  // Test merge sort
  let merge_sorted = SortUtil::merge_sort(unsorted_array)
  assert_eq(merge_sorted, expected_sorted)
  
  // Test heap sort
  let heap_sorted = SortUtil::heap_sort(unsorted_array)
  assert_eq(heap_sorted, expected_sorted)
  
  // Test insertion sort
  let insertion_sorted = SortUtil::insertion_sort(unsorted_array)
  assert_eq(insertion_sorted, expected_sorted)
  
  // Test selection sort
  let selection_sorted = SortUtil::selection_sort(unsorted_array)
  assert_eq(selection_sorted, expected_sorted)
  
  // Test empty array
  let empty_array = []
  assert_eq(SortUtil::bubble_sort(empty_array), [])
  assert_eq(SortUtil::quick_sort(empty_array), [])
  assert_eq(SortUtil::merge_sort(empty_array), [])
  assert_eq(SortUtil::heap_sort(empty_array), [])
  assert_eq(SortUtil::insertion_sort(empty_array), [])
  assert_eq(SortUtil::selection_sort(empty_array), [])
  
  // Test single element array
  let single_element = [42]
  assert_eq(SortUtil::bubble_sort(single_element), [42])
  assert_eq(SortUtil::quick_sort(single_element), [42])
  assert_eq(SortUtil::merge_sort(single_element), [42])
  assert_eq(SortUtil::heap_sort(single_element), [42])
  assert_eq(SortUtil::insertion_sort(single_element), [42])
  assert_eq(SortUtil::selection_sort(single_element), [42])
}

// Test 2: Search Algorithms
test "search_algorithms" {
  let sorted_array = [2, 5, 8, 12, 16, 23, 38, 56, 72, 91]
  
  // Test linear search
  assert_eq(SearchUtil::linear_search(sorted_array, 23), 5)
  assert_eq(SearchUtil::linear_search(sorted_array, 91), 9)
  assert_eq(SearchUtil::linear_search(sorted_array, 2), 0)
  assert_eq(SearchUtil::linear_search(sorted_array, 100), -1) // Not found
  
  // Test binary search
  assert_eq(SearchUtil::binary_search(sorted_array, 23), 5)
  assert_eq(SearchUtil::binary_search(sorted_array, 91), 9)
  assert_eq(SearchUtil::binary_search(sorted_array, 2), 0)
  assert_eq(SearchUtil::binary_search(sorted_array, 100), -1) // Not found
  
  // Test interpolation search
  assert_eq(SearchUtil::interpolation_search(sorted_array, 23), 5)
  assert_eq(SearchUtil::interpolation_search(sorted_array, 91), 9)
  assert_eq(SearchUtil::interpolation_search(sorted_array, 2), 0)
  assert_eq(SearchUtil::interpolation_search(sorted_array, 100), -1) // Not found
  
  // Test exponential search
  assert_eq(SearchUtil::exponential_search(sorted_array, 23), 5)
  assert_eq(SearchUtil::exponential_search(sorted_array, 91), 9)
  assert_eq(SearchUtil::exponential_search(sorted_array, 2), 0)
  assert_eq(SearchUtil::exponential_search(sorted_array, 100), -1) // Not found
  
  // Test jump search
  assert_eq(SearchUtil::jump_search(sorted_array, 23), 5)
  assert_eq(SearchUtil::jump_search(sorted_array, 91), 9)
  assert_eq(SearchUtil::jump_search(sorted_array, 2), 0)
  assert_eq(SearchUtil::jump_search(sorted_array, 100), -1) // Not found
  
  // Test string search
  let text = "This is a sample text for searching patterns"
  assert_eq(SearchUtil::substring_search(text, "sample"), 10)
  assert_eq(SearchUtil::substring_search(text, "patterns"), 35)
  assert_eq(SearchUtil::substring_search(text, "notfound"), -1)
  
  // Test KMP algorithm
  let kmp_pattern = "ABCDABD"
  let kmp_text = "ABC ABCDAB ABCDABCDABDE"
  assert_eq(SearchUtil::kmp_search(kmp_text, kmp_pattern), 15)
  assert_eq(SearchUtil::kmp_search(kmp_text, "XYZ"), -1)
}

// Test 3: Graph Algorithms
test "graph_algorithms" {
  // Create a graph for testing
  let mut graph = Graph::new()
  
  // Add vertices
  let vertex_a = Graph::add_vertex(graph, "A")
  let vertex_b = Graph::add_vertex(graph, "B")
  let vertex_c = Graph::add_vertex(graph, "C")
  let vertex_d = Graph::add_vertex(graph, "D")
  let vertex_e = Graph::add_vertex(graph, "E")
  
  // Add edges
  Graph::add_edge(graph, vertex_a, vertex_b, 10)
  Graph::add_edge(graph, vertex_a, vertex_c, 20)
  Graph::add_edge(graph, vertex_b, vertex_d, 15)
  Graph::add_edge(graph, vertex_c, vertex_d, 30)
  Graph::add_edge(graph, vertex_b, vertex_e, 25)
  Graph::add_edge(graph, vertex_d, vertex_e, 35)
  
  // Test BFS traversal
  let bfs_result = Graph::bfs_traversal(graph, vertex_a)
  assert_eq(bfs_result, ["A", "B", "C", "D", "E"])
  
  // Test DFS traversal
  let dfs_result = Graph::dfs_traversal(graph, vertex_a)
  assert_eq(dfs_result, ["A", "B", "D", "E", "C"])
  
  // Test Dijkstra's shortest path
  let shortest_path = Graph::dijkstra_shortest_path(graph, vertex_a, vertex_e)
  match shortest_path {
    Some((distance, path)) => {
      assert_eq(distance, 35) // A -> B -> E
      assert_eq(path, ["A", "B", "E"])
    }
    None => assert_true(false)
  }
  
  // Test Bellman-Ford algorithm
  let bellman_ford_path = Graph::bellman_ford_shortest_path(graph, vertex_a, vertex_e)
  match bellman_ford_path {
    Some((distance, path)) => {
      assert_eq(distance, 35) // A -> B -> E
      assert_eq(path, ["A", "B", "E"])
    }
    None => assert_true(false)
  }
  
  // Test Floyd-Warshall algorithm
  let all_pairs_shortest = Graph::floyd_warshall_all_pairs(graph)
  assert_eq(all_pairs_shortest.length(), 5) // 5 vertices
  
  // Test topological sort
  let mut dag = Graph::new()
  let v1 = Graph::add_vertex(dag, "1")
  let v2 = Graph::add_vertex(dag, "2")
  let v3 = Graph::add_vertex(dag, "3")
  let v4 = Graph::add_vertex(dag, "4")
  let v5 = Graph::add_vertex(dag, "5")
  
  Graph::add_edge(dag, v1, v2, 0)
  Graph::add_edge(dag, v1, v3, 0)
  Graph::add_edge(dag, v2, v4, 0)
  Graph::add_edge(dag, v3, v4, 0)
  Graph::add_edge(dag, v4, v5, 0)
  
  let topological_order = Graph::topological_sort(dag)
  assert_eq(topological_order, ["1", "2", "3", "4", "5"])
  
  // Test minimum spanning tree (Prim's algorithm)
  let mst = Graph::prim_minimum_spanning_tree(graph)
  assert_eq(mst.total_weight, 80) // A-B(10) + B-D(15) + A-C(20) + B-E(25)
}

// Test 4: Dynamic Programming
test "dynamic_programming" {
  // Test Fibonacci sequence
  assert_eq(DynamicProgramming::fibonacci(0), 0)
  assert_eq(DynamicProgramming::fibonacci(1), 1)
  assert_eq(DynamicProgramming::fibonacci(5), 5)
  assert_eq(DynamicProgramming::fibonacci(10), 55)
  
  // Test longest common subsequence
  let str1 = "AGGTAB"
  let str2 = "GXTXAYB"
  assert_eq(DynamicProgramming::longest_common_subsequence(str1, str2), "GTAB")
  
  // Test edit distance
  let word1 = "kitten"
  let word2 = "sitting"
  assert_eq(DynamicProgramming::edit_distance(word1, word2), 3)
  
  // Test coin change problem
  let coins = [1, 5, 10, 25]
  let amount = 30
  let min_coins = DynamicProgramming::coin_change(coins, amount)
  assert_eq(min_coins, 2) // 25 + 5
  
  // Test knapsack problem
  let weights = [10, 20, 30]
  let values = [60, 100, 120]
  let capacity = 50
  let max_value = DynamicProgramming::knapsack_01(weights, values, capacity)
  assert_eq(max_value, 220) // Items 1 and 2 (20+30 weight, 100+120 value)
  
  // Test longest increasing subsequence
  let sequence = [10, 22, 9, 33, 21, 50, 41, 60, 80]
  let lis_length = DynamicProgramming::longest_increasing_subsequence(sequence)
  assert_eq(lis_length, 6) // [10, 22, 33, 50, 60, 80]
  
  // Test matrix chain multiplication
  let matrices = [(10, 30), (30, 5), (5, 60)]
  let min_multiplications = DynamicProgramming::matrix_chain_multiplication(matrices)
  assert_eq(min_multiplications, 4500)
}

// Test 5: Tree Algorithms
test "tree_algorithms" {
  // Create a binary tree for testing
  let mut tree = BinaryTree::new()
  let root = BinaryTree::insert(tree, 50)
  BinaryTree::insert(tree, 30)
  BinaryTree::insert(tree, 70)
  BinaryTree::insert(tree, 20)
  BinaryTree::insert(tree, 40)
  BinaryTree::insert(tree, 60)
  BinaryTree::insert(tree, 80)
  
  // Test tree height
  assert_eq(BinaryTree::height(tree), 3)
  
  // Test tree diameter
  assert_eq(BinaryTree::diameter(tree), 5) // Longest path between any two nodes
  
  // Test tree is balanced
  assert_true(BinaryTree::is_balanced(tree))
  
  // Test tree is complete
  assert_true(BinaryTree::is_complete(tree))
  
  // Test tree is full
  assert_true(BinaryTree::is_full(tree))
  
  // Test tree is perfect
  assert_true(BinaryTree::is_perfect(tree))
  
  // Test lowest common ancestor
  let node1 = BinaryTree::find_node(tree, 20)
  let node2 = BinaryTree::find_node(tree, 40)
  let lca = BinaryTree::lowest_common_ancestor(tree, node1, node2)
  match lca {
    Some(node) => assert_eq(BinaryTree::node_value(node), 30)
    None => assert_true(false)
  }
  
  // Test tree serialization
  let serialized = BinaryTree::serialize(tree)
  assert_true(serialized.length() > 0)
  
  // Test tree deserialization
  let deserialized_tree = BinaryTree::deserialize(serialized)
  assert_true(BinaryTree::equals(tree, deserialized_tree))
  
  // Test tree to list conversion
  let inorder_list = BinaryTree::to_inorder_list(tree)
  assert_eq(inorder_list, [20, 30, 40, 50, 60, 70, 80])
  
  let preorder_list = BinaryTree::to_preorder_list(tree)
  assert_eq(preorder_list, [50, 30, 20, 40, 70, 60, 80])
  
  let postorder_list = BinaryTree::to_postorder_list(tree)
  assert_eq(postorder_list, [20, 40, 30, 60, 80, 70, 50])
}

// Test 6: String Algorithms
test "string_algorithms" {
  // Test string reversal
  assert_eq(StringAlgorithms::reverse("hello"), "olleh")
  assert_eq(StringAlgorithms::reverse(""), "")
  assert_eq(StringAlgorithms::reverse("a"), "a")
  
  // Test palindrome check
  assert_true(StringAlgorithms::is_palindrome("racecar"))
  assert_true(StringAlgorithms::is_palindrome("A man, a plan, a canal: Panama"))
  assert_false(StringAlgorithms::is_palindrome("hello"))
  
  // Test anagram check
  assert_true(StringAlgorithms::are_anagrams("listen", "silent"))
  assert_true(StringAlgorithms::are_anagrams("triangle", "integral"))
  assert_false(StringAlgorithms::are_anagrams("hello", "world"))
  
  // Test string permutations
  let permutations = StringAlgorithms::permutations("abc")
  assert_eq(permutations.length(), 6) // 3! = 6
  assert_true(permutations.contains("abc"))
  assert_true(permutations.contains("acb"))
  assert_true(permutations.contains("bac"))
  assert_true(permutations.contains("bca"))
  assert_true(permutations.contains("cab"))
  assert_true(permutations.contains("cba"))
  
  // Test longest palindrome substring
  assert_eq(StringAlgorithms::longest_palindrome_substring("babad"), "bab")
  assert_eq(StringAlgorithms::longest_palindrome_substring("cbbd"), "bb")
  assert_eq(StringAlgorithms::longest_palindrome_substring("a"), "a")
  
  // Test longest common prefix
  let strings = ["flower", "flow", "flight"]
  assert_eq(StringAlgorithms::longest_common_prefix(strings), "fl")
  
  let strings2 = ["dog", "racecar", "car"]
  assert_eq(StringAlgorithms::longest_common_prefix(strings2), "")
  
  // Test string compression
  assert_eq(StringAlgorithms::compress("aabcccccaaa"), "a2b1c5a3")
  assert_eq(StringAlgorithms::compress("abc"), "abc") // Not shorter after compression
}

// Test 7: Mathematical Algorithms
test "mathematical_algorithms" {
  // Test greatest common divisor
  assert_eq(MathAlgorithms::gcd(48, 18), 6)
  assert_eq(MathAlgorithms::gcd(17, 5), 1)
  assert_eq(MathAlgorithms::gcd(0, 5), 5)
  
  // Test least common multiple
  assert_eq(MathAlgorithms::lcm(12, 18), 36)
  assert_eq(MathAlgorithms::lcm(5, 7), 35)
  
  // Test prime number check
  assert_true(MathAlgorithms::is_prime(2))
  assert_true(MathAlgorithms::is_prime(3))
  assert_true(MathAlgorithms::is_prime(17))
  assert_false(MathAlgorithms::is_prime(1))
  assert_false(MathAlgorithms::is_prime(4))
  assert_false(MathAlgorithms::is_prime(9))
  
  // Test prime factorization
  assert_eq(MathAlgorithms::prime_factors(12), [2, 2, 3])
  assert_eq(MathAlgorithms::prime_factors(17), [17])
  assert_eq(MathAlgorithms::prime_factors(1), [])
  
  // Test factorial
  assert_eq(MathAlgorithms::factorial(0), 1)
  assert_eq(MathAlgorithms::factorial(1), 1)
  assert_eq(MathAlgorithms::factorial(5), 120)
  assert_eq(MathAlgorithms::factorial(10), 3628800)
  
  // Test power function
  assert_eq(MathAlgorithms::power(2, 0), 1)
  assert_eq(MathAlgorithms::power(2, 3), 8)
  assert_eq(MathAlgorithms::power(5, 4), 625)
  
  // Test square root
  assert_eq(MathAlgorithms::square_root(4), 2)
  assert_eq(MathAlgorithms::square_root(9), 3)
  assert_eq(MathAlgorithms::square_root(16), 4)
  
  // Test nth root
  assert_eq(MathAlgorithms::nth_root(8, 3), 2) // Cube root of 8
  assert_eq(MathAlgorithms::nth_root(27, 3), 3) // Cube root of 27
  
  // Test sieve of Eratosthenes
  let primes = MathAlgorithms::sieve_of_eratosthenes(10)
  assert_eq(primes, [2, 3, 5, 7])
  
  let primes2 = MathAlgorithms::sieve_of_eratosthenes(20)
  assert_eq(primes2, [2, 3, 5, 7, 11, 13, 17, 19])
}