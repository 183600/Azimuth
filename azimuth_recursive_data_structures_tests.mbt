// Azimuth Recursive Data Structures Test Suite
// 递归数据结构和算法测试用例

// 测试1: 递归链表操作
test "递归链表操作测试" {
  // 定义递归链表结构
  enum List[T] {
    Nil
    Cons(T, List[T])
  }
  
  // 创建链表
  let empty_list = List::Nil
  let list1 = List::Cons(1, List::Cons(2, List::Cons(3, List::Nil)))
  let list2 = List::Cons("a", List::Cons("b", List::Cons("c", List::Nil)))
  
  // 递归计算链表长度
  fn list_length[T](lst : List[T]) -> Int {
    match lst {
      List::Nil => 0
      List::Cons(_, tail) => 1 + list_length(tail)
    }
  }
  
  // 递归反转链表
  fn reverse_list[T](lst : List[T]) -> List[T] {
    fn reverse_helper[T](lst : List[T], acc : List[T]) -> List[T] {
      match lst {
        List::Nil => acc
        List::Cons(head, tail) => reverse_helper(tail, List::Cons(head, acc))
      }
    }
    reverse_helper(lst, List::Nil)
  }
  
  // 递归映射函数
  fn map_list[T, U](lst : List[T], f : (T) -> U) -> List[U] {
    match lst {
      List::Nil => List::Nil
      List::Cons(head, tail) => List::Cons(f(head), map_list(tail, f))
    }
  }
  
  // 递归过滤函数
  fn filter_list[T](lst : List[T], predicate : (T) -> Bool) -> List[T] {
    match lst {
      List::Nil => List::Nil
      List::Cons(head, tail) => 
        if predicate(head) {
          List::Cons(head, filter_list(tail, predicate))
        } else {
          filter_list(tail, predicate)
        }
    }
  }
  
  // 测试链表长度
  assert_eq(list_length(empty_list), 0)
  assert_eq(list_length(list1), 3)
  assert_eq(list_length(list2), 3)
  
  // 测试链表反转
  let reversed_list1 = reverse_list(list1)
  assert_eq(list_length(reversed_list1), 3)
  
  // 测试链表映射
  let doubled_list = map_list(list1, fn(x) { x * 2 })
  assert_eq(list_length(doubled_list), 3)
  
  // 测试链表过滤
  let filtered_list = filter_list(list1, fn(x) { x > 1 })
  assert_eq(list_length(filtered_list), 2)
}

// 测试2: 递归树结构操作
test "递归树结构操作测试" {
  // 定义二叉树结构
  enum Tree[T] {
    Empty
    Node(T, Tree[T], Tree[T])
  }
  
  // 创建示例树
  let tree = Tree::Node(5, 
    Tree::Node(3, Tree::Node(1, Tree::Empty, Tree::Empty), Tree::Node(4, Tree::Empty, Tree::Empty)),
    Tree::Node(7, Tree::Node(6, Tree::Empty, Tree::Empty), Tree::Node(8, Tree::Empty, Tree::Empty))
  )
  
  // 递归计算树的高度
  fn tree_height[T](tree : Tree[T]) -> Int {
    match tree {
      Tree::Empty => 0
      Tree::Node(_, left, right) => {
        let left_height = tree_height(left)
        let right_height = tree_height(right)
        1 + (if left_height > right_height { left_height } else { right_height })
      }
    }
  }
  
  // 递归计算树的节点数
  fn tree_size[T](tree : Tree[T]) -> Int {
    match tree {
      Tree::Empty => 0
      Tree::Node(_, left, right) => 1 + tree_size(left) + tree_size(right)
    }
  }
  
  // 递归中序遍历
  fn inorder_traversal[T](tree : Tree[T]) -> List[T] {
    match tree {
      Tree::Empty => List::Nil
      Tree::Node(value, left, right) => {
        fn append_lists[T](list1 : List[T], list2 : List[T]) -> List[T] {
          match list1 {
            List::Nil => list2
            List::Cons(head, tail) => List::Cons(head, append_lists(tail, list2))
          }
        }
        let left_result = inorder_traversal(left)
        let right_result = inorder_traversal(right)
        append_lists(left_result, List::Cons(value, right_result))
      }
    }
  }
  
  // 递归查找元素
  fn tree_contains[T](tree : Tree[T], value : T, cmp : (T, T) -> Bool) -> Bool {
    match tree {
      Tree::Empty => false
      Tree::Node(node_value, left, right) => {
        if cmp(node_value, value) {
          true
        } else if cmp(value, node_value) {
          tree_contains(left, value, cmp)
        } else {
          tree_contains(right, value, cmp)
        }
      }
    }
  }
  
  // 测试树的高度和大小
  assert_eq(tree_height(tree), 3)
  assert_eq(tree_size(tree), 7)
  
  // 测试中序遍历
  let inorder_result = inorder_traversal(tree)
  assert_eq(list_length(inorder_result), 7)
  
  // 测试元素查找
  let int_compare = fn(a : Int, b : Int) { a == b }
  assert_true(tree_contains(tree, 5, int_compare))
  assert_true(tree_contains(tree, 1, int_compare))
  assert_true(tree_contains(tree, 8, int_compare))
  assert_false(tree_contains(tree, 10, int_compare))
}

// 测试3: 递归图遍历算法
test "递归图遍历算法测试" {
  // 定义图结构（使用邻接表表示）
  type Graph = Array[(Int, Array[Int])]
  
  // 创建示例图
  let graph = [
    (1, [2, 3]),
    (2, [4, 5]),
    (3, [6]),
    (4, []),
    (5, [6]),
    (6, [])
  ]
  
  // 递归深度优先搜索
  fn dfs(graph : Graph, node : Int, visited : Array[Int]) -> Array[Int] {
    // 检查节点是否已访问
    fn is_visited(visited : Array[Int], node : Int) -> Bool {
      for v in visited {
        if v == node { return true }
      }
      false
    }
    
    if is_visited(visited, node) {
      visited
    } else {
      let new_visited = visited.push(node)
      
      // 获取节点的邻居
      fn get_neighbors(graph : Graph, node : Int) -> Array[Int] {
        for (n, neighbors) in graph {
          if n == node { return neighbors }
        }
        []
      }
      
      let neighbors = get_neighbors(graph, node)
      
      // 递归访问所有未访问的邻居
      fn visit_neighbors(graph : Graph, neighbors : Array[Int], visited : Array[Int]) -> Array[Int] {
        match neighbors {
          [] => visited
          [head, ..tail] => {
            let updated_visited = dfs(graph, head, visited)
            visit_neighbors(graph, tail, updated_visited)
          }
        }
      }
      
      visit_neighbors(graph, neighbors, new_visited)
    }
  }
  
  // 递归查找路径
  fn find_path(graph : Graph, current : Int, target : Int, path : Array[Int]) -> Option[Array[Int]] {
    if current == target {
      Some(path.push(current))
    } else {
      // 检查是否已经在路径中（避免循环）
      fn in_path(path : Array[Int], node : Int) -> Bool {
        for p in path {
          if p == node { return true }
        }
        false
      }
      
      if in_path(path, current) {
        None
      } else {
        let new_path = path.push(current)
        
        // 获取节点的邻居
        fn get_neighbors(graph : Graph, node : Int) -> Array[Int] {
          for (n, neighbors) in graph {
            if n == node { return neighbors }
          }
          []
        }
        
        let neighbors = get_neighbors(graph, current)
        
        // 尝试每个邻居
        fn try_neighbors(graph : Graph, neighbors : Array[Int], target : Int, path : Array[Int]) -> Option[Array[Int]] {
          match neighbors {
            [] => None
            [head, ..tail] => {
              match find_path(graph, head, target, path) {
                Some(found_path) => Some(found_path)
                None => try_neighbors(graph, tail, target, path)
              }
            }
          }
        }
        
        try_neighbors(graph, neighbors, target, new_path)
      }
    }
  }
  
  // 测试DFS遍历
  let dfs_result = dfs(graph, 1, [])
  assert_eq(dfs_result.length(), 6)
  
  // 测试路径查找
  let path_1_to_6 = find_path(graph, 1, 6, [])
  assert_true(path_1_to_6.is_some())
  assert_eq(path_1_to_6.unwrap().length(), 3)
  
  let path_4_to_5 = find_path(graph, 4, 5, [])
  assert_true(path_4_to_5.is_none())  // 4到5没有路径
}

// 测试4: 递归分治算法
test "递归分治算法测试" {
  // 递归归并排序
  fn merge_sort(arr : Array[Int]) -> Array[Int] {
    if arr.length() <= 1 {
      arr
    } else {
      let mid = arr.length() / 2
      let left = merge_sort(arr.slice(0, mid))
      let right = merge_sort(arr.slice(mid, arr.length()))
      merge(left, right)
    }
  }
  
  fn merge(left : Array[Int], right : Array[Int]) -> Array[Int] {
    fn merge_helper(left : Array[Int], right : Array[Int], result : Array[Int]) -> Array[Int] {
      match (left, right) {
        ([], right) => result.concat(right)
        (left, []) => result.concat(left)
        ([l_head, ..l_tail], [r_head, ..r_tail]) => {
          if l_head <= r_head {
            merge_helper(l_tail, right, result.push(l_head))
          } else {
            merge_helper(left, r_tail, result.push(r_head))
          }
        }
      }
    }
    merge_helper(left, right, [])
  }
  
  // 递归二分查找
  fn binary_search(arr : Array[Int], target : Int, left : Int, right : Int) -> Bool {
    if left > right {
      false
    } else {
      let mid = (left + right) / 2
      if arr[mid] == target {
        true
      } else if arr[mid] > target {
        binary_search(arr, target, left, mid - 1)
      } else {
        binary_search(arr, target, mid + 1, right)
      }
    }
  }
  
  // 递归最大子数组和
  fn max_subarray(arr : Array[Int]) -> Int {
    fn max_subarray_helper(arr : Array[Int], left : Int, right : Int) -> Int {
      if left == right {
        arr[left]
      } else {
        let mid = (left + right) / 2
        let left_max = max_subarray_helper(arr, left, mid)
        let right_max = max_subarray_helper(arr, mid + 1, right)
        let cross_max = max_crossing_subarray(arr, left, mid, right)
        
        // 返回三个值中的最大值
        if left_max >= right_max && left_max >= cross_max {
          left_max
        } else if right_max >= left_max && right_max >= cross_max {
          right_max
        } else {
          cross_max
        }
      }
    }
    
    fn max_crossing_subarray(arr : Array[Int], left : Int, mid : Int, right : Int) -> Int {
      let mut left_sum = -2147483648  // Int最小值
      let mut sum = 0
      for i in mid..left {
        sum = sum + arr[i]
        if sum > left_sum {
          left_sum = sum
        }
      }
      
      let mut right_sum = -2147483648  // Int最小值
      sum = 0
      for i in mid + 1..right {
        sum = sum + arr[i]
        if sum > right_sum {
          right_sum = sum
        }
      }
      
      left_sum + right_sum
    }
    
    if arr.length() == 0 {
      0
    } else {
      max_subarray_helper(arr, 0, arr.length() - 1)
    }
  }
  
  // 测试数据
  let unsorted_array = [5, 2, 8, 1, 9, 3, 7, 4, 6]
  let sorted_array = merge_sort(unsorted_array)
  
  // 测试归并排序
  assert_eq(sorted_array.length(), 9)
  for i in 0..8 {
    assert_true(sorted_array[i] <= sorted_array[i + 1])
  }
  
  // 测试二分查找
  assert_true(binary_search(sorted_array, 1, 0, sorted_array.length() - 1))
  assert_true(binary_search(sorted_array, 5, 0, sorted_array.length() - 1))
  assert_true(binary_search(sorted_array, 9, 0, sorted_array.length() - 1))
  assert_false(binary_search(sorted_array, 10, 0, sorted_array.length() - 1))
  
  // 测试最大子数组和
  let test_array = [-2, 1, -3, 4, -1, 2, 1, -5, 4]
  let max_sum = max_subarray(test_array)
  assert_eq(max_sum, 6)  // 子数组 [4, -1, 2, 1] 的和
}

// 测试5: 递归动态规划
test "递归动态规划测试" {
  // 带记忆化的斐波那契数列
  fn fibonacci(n : Int, memo : Array[Option[Int]]) -> Int {
    match memo[n] {
      Some(value) => value
      None => {
        let result = if n <= 1 {
          n
        } else {
          fibonacci(n - 1, memo) + fibonacci(n - 2, memo)
        }
        // 更新记忆化数组
        memo[n] = Some(result)
        result
      }
    }
  }
  
  // 带记忆化的最长公共子序列
  fn lcs_length(s1 : String, s2 : String, i : Int, j : Int, memo : Array[Array[Option[Int]]]) -> Int {
    if i == 0 || j == 0 {
      0
    } else {
      match memo[i][j] {
        Some(value) => value
        None => {
          let result = if s1[i - 1] == s2[j - 1] {
            1 + lcs_length(s1, s2, i - 1, j - 1, memo)
          } else {
            let left = lcs_length(s1, s2, i - 1, j, memo)
            let right = lcs_length(s1, s2, i, j - 1, memo)
            if left > right { left } else { right }
          }
          memo[i][j] = Some(result)
          result
        }
      }
    }
  }
  
  // 带记忆化的背包问题
  fn knapsack(weights : Array[Int], values : Array[Int], capacity : Int, n : Int, memo : Array[Array[Option[Int]]]) -> Int {
    if n == 0 || capacity == 0 {
      0
    } else {
      match memo[n][capacity] {
        Some(value) => value
        None => {
          let result = if weights[n - 1] > capacity {
            knapsack(weights, values, capacity, n - 1, memo)
          } else {
            let without_item = knapsack(weights, values, capacity, n - 1, memo)
            let with_item = values[n - 1] + knapsack(weights, values, capacity - weights[n - 1], n - 1, memo)
            if with_item > without_item { with_item } else { without_item }
          }
          memo[n][capacity] = Some(result)
          result
        }
      }
    }
  }
  
  // 测试斐波那契数列
  let fib_memo = Array::filled(20, None)
  assert_eq(fibonacci(10, fib_memo), 55)
  assert_eq(fibonacci(15, fib_memo), 610)
  
  // 测试最长公共子序列
  let s1 = "AGGTAB"
  let s2 = "GXTXAYB"
  let lcs_memo = Array::filled(s1.length() + 1, Array::filled(s2.length() + 1, None))
  let lcs_result = lcs_length(s1, s2, s1.length(), s2.length(), lcs_memo)
  assert_eq(lcs_result, 4)  // "GTAB"
  
  // 测试背包问题
  let weights = [1, 3, 4, 5]
  let values = [1, 4, 5, 7]
  let capacity = 7
  let knapsack_memo = Array::filled(weights.length() + 1, Array::filled(capacity + 1, None))
  let knapsack_result = knapsack(weights, values, capacity, weights.length(), knapsack_memo)
  assert_eq(knapsack_result, 9)  // 选择重量3和4的物品
}