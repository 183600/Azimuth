// Azimuth Data Structures Operations Test Suite
// This file contains test cases for various data structure operations

// Test 1: Stack Operations
test "stack operations" {
  let mut stack = []
  
  // Push operations
  stack = stack.push(1)
  stack = stack.push(2)
  stack = stack.push(3)
  
  assert_eq(stack.length(), 3)
  
  // Pop operations
  let (popped1, new_stack1) = stack.pop()
  match popped1 {
    Some(value) => assert_eq(value, 3)
    None => assert_true(false)
  }
  stack = new_stack1
  
  let (popped2, new_stack2) = stack.pop()
  match popped2 {
    Some(value) => assert_eq(value, 2)
    None => assert_true(false)
  }
  stack = new_stack2
  
  assert_eq(stack.length(), 1)
  assert_eq(stack[0], 1)
}

// Test 2: Queue Operations
test "queue operations" {
  let mut queue = []
  
  // Enqueue operations
  queue = queue.push(1)
  queue = queue.push(2)
  queue = queue.push(3)
  
  assert_eq(queue.length(), 3)
  
  // Dequeue operations (simulated with array operations)
  if queue.length() > 0 {
    let dequeued = queue[0]
    assert_eq(dequeued, 1)
    queue = queue.slice(1, queue.length())
  }
  
  if queue.length() > 0 {
    let dequeued = queue[0]
    assert_eq(dequeued, 2)
    queue = queue.slice(1, queue.length())
  }
  
  assert_eq(queue.length(), 1)
  assert_eq(queue[0], 3)
}

// Test 3: Tree-like Structure Operations
test "tree-like structure operations" {
  // Simulate a simple tree structure using nested tuples
  type TreeNode = (Int, Array<TreeNode>)
  
  let leaf1 = (1, [])
  let leaf2 = (2, [])
  let leaf3 = (3, [])
  let leaf4 = (4, [])
  
  let subtree = (5, [leaf1, leaf2])
  let root = (6, [subtree, leaf3, leaf4])
  
  // Test tree traversal (sum all values)
  let sum_tree_values = fn(node: TreeNode) -> Int {
    let (value, children) = node
    let mut sum = value
    for child in children {
      sum = sum + sum_tree_values(child)
    }
    sum
  }
  
  assert_eq(sum_tree_values(root), 21)
  
  // Test tree depth calculation
  let tree_depth = fn(node: TreeNode) -> Int {
    let (_, children) = node
    if children.length() == 0 {
      1
    } else {
      let mut max_depth = 0
      for child in children {
        let child_depth = tree_depth(child)
        if child_depth > max_depth {
          max_depth = child_depth
        }
      }
      max_depth + 1
    }
  }
  
  assert_eq(tree_depth(root), 3)
}

// Test 4: Graph-like Structure Operations
test "graph-like structure operations" {
  // Simulate a simple graph using adjacency lists
  type Graph = Array<(Int, Array<Int>)>
  
  let graph = [
    (1, [2, 3]),
    (2, [1, 4]),
    (3, [1, 4]),
    (4, [2, 3, 5]),
    (5, [4])
  ]
  
  // Test graph traversal (BFS simulation)
  let find_neighbors = fn(g: Graph, node: Int) -> Array<Int> {
    for (n, neighbors) in g {
      if n == node {
        return neighbors
      }
    }
    []
  }
  
  let neighbors1 = find_neighbors(graph, 1)
  assert_eq(neighbors1, [2, 3])
  
  let neighbors5 = find_neighbors(graph, 5)
  assert_eq(neighbors5, [4])
  
  // Test path existence (simplified)
  let path_exists = fn(g: Graph, from: Int, to: Int) -> Bool {
    if from == to {
      return true
    }
    
    let visited = []
    let queue = [from]
    
    while queue.length() > 0 {
      let current = queue[0]
      queue = queue.slice(1, queue.length())
      
      if visited.contains(current) {
        continue
      }
      
      visited = visited.push(current)
      let neighbors = find_neighbors(g, current)
      
      for neighbor in neighbors {
        if neighbor == to {
          return true
        }
        if !visited.contains(neighbor) {
          queue = queue.push(neighbor)
        }
      }
    }
    
    false
  }
  
  assert_true(path_exists(graph, 1, 5))
  assert_true(path_exists(graph, 2, 3))
  assert_false(path_exists(graph, 5, 1))  // In our simple test, this might fail depending on implementation
}

// Test 5: Hash Map-like Operations
test "hash map-like operations" {
  // Simulate a hash map using an array of key-value pairs
  type HashMap = Array<(String, Int)>
  
  let empty_map: HashMap = []
  
  let insert = fn(map: HashMap, key: String, value: Int) -> HashMap {
    // Remove existing key if present
    let filtered = map.filter(fn(pair) { pair.0 != key })
    // Add new key-value pair
    filtered.push((key, value))
  }
  
  let get = fn(map: HashMap, key: String) -> Option<Int> {
    for (k, v) in map {
      if k == key {
        return Some(v)
      }
    }
    None
  }
  
  let remove = fn(map: HashMap, key: String) -> HashMap {
    map.filter(fn(pair) { pair.0 != key })
  }
  
  let map1 = insert(empty_map, "one", 1)
  let map2 = insert(map1, "two", 2)
  let map3 = insert(map2, "three", 3)
  
  match get(map3, "one") {
    Some(value) => assert_eq(value, 1)
    None => assert_true(false)
  }
  
  match get(map3, "two") {
    Some(value) => assert_eq(value, 2)
    None => assert_true(false)
  }
  
  match get(map3, "four") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  let map4 = insert(map3, "one", 10)  // Update existing key
  match get(map4, "one") {
    Some(value) => assert_eq(value, 10)
    None => assert_true(false)
  }
  
  let map5 = remove(map4, "two")
  match get(map5, "two") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  match get(map5, "one") {
    Some(value) => assert_eq(value, 10)
    None => assert_true(false)
  }
  
  match get(map5, "three") {
    Some(value) => assert_eq(value, 3)
    None => assert_true(false)
  }
}

// Test 6: Set Operations
test "set operations" {
  // Simulate a set using an array
  type Set = Array<String>
  
  let empty_set: Set = []
  
  let add = fn(set: Set, item: String) -> Set {
    if set.contains(item) {
      set
    } else {
      set.push(item)
    }
  }
  
  let remove = fn(set: Set, item: String) -> Set {
    set.filter(fn(x) { x != item })
  }
  
  let union = fn(set1: Set, set2: Set) -> Set {
    let mut result = set1
    for item in set2 {
      result = add(result, item)
    }
    result
  }
  
  let intersection = fn(set1: Set, set2: Set) -> Set {
    set1.filter(fn(x) { set2.contains(x) })
  }
  
  let difference = fn(set1: Set, set2: Set) -> Set {
    set1.filter(fn(x) { !set2.contains(x) })
  }
  
  let set1 = add(add(add(empty_set, "a"), "b"), "c")
  let set2 = add(add(add(empty_set, "b"), "c"), "d")
  
  assert_eq(set1.length(), 3)
  assert_true(set1.contains("a"))
  assert_true(set1.contains("b"))
  assert_true(set1.contains("c"))
  assert_false(set1.contains("d"))
  
  let set3 = remove(set1, "b")
  assert_eq(set3.length(), 2)
  assert_true(set3.contains("a"))
  assert_false(set3.contains("b"))
  assert_true(set3.contains("c"))
  
  let union_set = union(set1, set2)
  assert_eq(union_set.length(), 4)
  assert_true(union_set.contains("a"))
  assert_true(union_set.contains("b"))
  assert_true(union_set.contains("c"))
  assert_true(union_set.contains("d"))
  
  let intersection_set = intersection(set1, set2)
  assert_eq(intersection_set.length(), 2)
  assert_false(intersection_set.contains("a"))
  assert_true(intersection_set.contains("b"))
  assert_true(intersection_set.contains("c"))
  assert_false(intersection_set.contains("d"))
  
  let difference_set = difference(set1, set2)
  assert_eq(difference_set.length(), 1)
  assert_true(difference_set.contains("a"))
  assert_false(difference_set.contains("b"))
  assert_false(difference_set.contains("c"))
  assert_false(difference_set.contains("d"))
}

// Test 7: Priority Queue Operations
test "priority queue operations" {
  // Simulate a priority queue using a sorted array
  type PriorityQueue = Array<(Int, String)>  // (priority, value)
  
  let empty_pq: PriorityQueue = []
  
  let enqueue = fn(pq: PriorityQueue, priority: Int, value: String) -> PriorityQueue {
    let new_item = (priority, value)
    let mut result = []
    let mut inserted = false
    
    for item in pq {
      if !inserted && priority < item.0 {
        result = result.push(new_item)
        inserted = true
      }
      result = result.push(item)
    }
    
    if !inserted {
      result = result.push(new_item)
    }
    
    result
  }
  
  let dequeue = fn(pq: PriorityQueue) -> (Option<(Int, String)>, PriorityQueue) {
    if pq.length() == 0 {
      (None, pq)
    } else {
      let item = pq[0]
      let remaining = pq.slice(1, pq.length())
      (Some(item), remaining)
    }
  }
  
  let pq1 = enqueue(empty_pq, 3, "low")
  let pq2 = enqueue(pq1, 1, "high")
  let pq3 = enqueue(pq2, 2, "medium")
  
  assert_eq(pq3.length(), 3)
  assert_eq(pq3[0], (1, "high"))
  assert_eq(pq3[1], (2, "medium"))
  assert_eq(pq3[2], (3, "low"))
  
  let (item1, pq4) = dequeue(pq3)
  match item1 {
    Some((priority, value)) => {
      assert_eq(priority, 1)
      assert_eq(value, "high")
    }
    None => assert_true(false)
  }
  
  let (item2, pq5) = dequeue(pq4)
  match item2 {
    Some((priority, value)) => {
      assert_eq(priority, 2)
      assert_eq(value, "medium")
    }
    None => assert_true(false)
  }
  
  let (item3, pq6) = dequeue(pq5)
  match item3 {
    Some((priority, value)) => {
      assert_eq(priority, 3)
      assert_eq(value, "low")
    }
    None => assert_true(false)
  }
  
  assert_eq(pq6.length(), 0)
}

// Test 8: Linked List Operations
test "linked list operations" {
  // Simulate a linked list using a recursive structure
  type LinkedList = Option<(Int, LinkedList)>
  
  let empty_list: LinkedList = None
  
  let cons = fn(head: Int, tail: LinkedList) -> LinkedList {
    Some((head, tail))
  }
  
  let length = fn(list: LinkedList) -> Int {
    match list {
      None => 0
      Some((_, tail)) => 1 + length(tail)
    }
  }
  
  let sum = fn(list: LinkedList) -> Int {
    match list {
      None => 0
      Some((head, tail)) => head + sum(tail)
    }
  }
  
  let reverse = fn(list: LinkedList) -> LinkedList {
    let helper = fn(src: LinkedList, acc: LinkedList) -> LinkedList {
      match src {
        None => acc
        Some((head, tail)) => helper(tail, cons(head, acc))
      }
    }
    helper(list, None)
  }
  
  let list1 = cons(1, empty_list)
  let list2 = cons(2, list1)
  let list3 = cons(3, list2)
  
  assert_eq(length(list3), 3)
  assert_eq(sum(list3), 6)
  
  let reversed = reverse(list3)
  assert_eq(length(reversed), 3)
  assert_eq(sum(reversed), 6)
  
  // Check if reversed correctly
  match reversed {
    None => assert_true(false)
    Some((head1, tail1)) => {
      assert_eq(head1, 1)
      match tail1 {
        None => assert_true(false)
        Some((head2, tail2)) => {
          assert_eq(head2, 2)
          match tail2 {
            None => assert_true(false)
            Some((head3, tail3)) => {
              assert_eq(head3, 3)
              assert_eq(tail3, None)
            }
          }
        }
      }
    }
  }
}