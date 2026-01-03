// Azimuth 数据结构与算法测试
// 专注于测试基本数据结构和算法操作

// 测试1: 数组排序算法
test "数组排序算法测试" {
  let unsorted_array = [5, 2, 8, 1, 9, 3]
  // 简单冒泡排序实现
  let sorted_array = bubble_sort(unsorted_array)
  assert_eq(sorted_array, [1, 2, 3, 5, 8, 9])
  
  let empty_array = []
  let sorted_empty = bubble_sort(empty_array)
  assert_eq(sorted_empty, [])
  
  let single_element = [42]
  let sorted_single = bubble_sort(single_element)
  assert_eq(sorted_single, [42])
}

// 简单冒泡排序实现
fn bubble_sort(arr : Array[Int]) -> Array[Int] {
  let n = arr.length()
  let result = arr.copy()
  
  for i = 0; i < n - 1; i = i + 1 {
    for j = 0; j < n - i - 1; j = j + 1 {
      if (result[j] > result[j + 1]) {
        let temp = result[j]
        result[j] = result[j + 1]
        result[j + 1] = temp
      }
    }
  }
  
  result
}

// 测试2: 链表操作
test "链表操作测试" {
  let empty_list = Nil
  assert_eq(list_length(empty_list), 0)
  
  let list1 = Cons(1, Nil)
  assert_eq(list_length(list1), 1)
  
  let list2 = Cons(1, Cons(2, Cons(3, Nil)))
  assert_eq(list_length(list2), 3)
  
  let list3 = list_reverse(list2)
  assert_eq(list3, Cons(3, Cons(2, Cons(1, Nil))))
}

type List[T] {
  Nil
  Cons(T, List[T])
}

fn list_length[T](lst : List[T]) -> Int {
  match lst {
    Nil => 0
    Cons(_, tail) => 1 + list_length(tail)
  }
}

fn list_reverse[T](lst : List[T]) -> List[T] {
  fn reverse_helper(original : List[T], reversed : List[T]) -> List[T] {
    match original {
      Nil => reversed
      Cons(head, tail) => reverse_helper(tail, Cons(head, reversed))
    }
  }
  reverse_helper(lst, Nil)
}

// 测试3: 二分查找算法
test "二分查找算法测试" {
  let sorted_array = [1, 3, 5, 7, 9, 11, 13, 15]
  
  assert_eq(binary_search(sorted_array, 7), true)
  assert_eq(binary_search(sorted_array, 1), true)
  assert_eq(binary_search(sorted_array, 15), true)
  assert_eq(binary_search(sorted_array, 8), false)
  assert_eq(binary_search(sorted_array, 0), false)
  assert_eq(binary_search(sorted_array, 16), false)
  
  let empty_array = []
  assert_eq(binary_search(empty_array, 5), false)
  
  let single_element = [10]
  assert_eq(binary_search(single_element, 10), true)
  assert_eq(binary_search(single_element, 5), false)
}

fn binary_search(arr : Array[Int], target : Int) -> Bool {
  let left = 0
  let right = arr.length() - 1
  
  while (left <= right) {
    let mid = (left + right) / 2
    let mid_value = arr[mid]
    
    if (mid_value == target) {
      return true
    } else if (mid_value < target) {
      left = mid + 1
    } else {
      right = mid - 1
    }
  }
  
  false
}

// 测试4: 哈希表操作
test "哈希表操作测试" {
  let empty_map = Map::new()
  assert_eq(Map::size(empty_map), 0)
  
  let map1 = Map::insert(empty_map, "key1", 100)
  assert_eq(Map::size(map1), 1)
  assert_eq(Map::get(map1, "key1"), Some(100))
  
  let map2 = Map::insert(map1, "key2", 200)
  assert_eq(Map::size(map2), 2)
  assert_eq(Map::get(map2, "key1"), Some(100))
  assert_eq(Map::get(map2, "key2"), Some(200))
  
  let map3 = Map::insert(map2, "key1", 150)  // 更新现有键
  assert_eq(Map::size(map3), 2)
  assert_eq(Map::get(map3, "key1"), Some(150))
  
  assert_eq(Map::get(map3, "nonexistent"), None)
}

// 简单的Map实现
type Map[K, V] = Array[(K, V)]

namespace Map {
  pub fn new[K, V]() -> Map[K, V] {
    []
  }
  
  pub fn size[K, V](map : Map[K, V]) -> Int {
    map.length()
  }
  
  pub fn get[K : Eq, V](map : Map[K, V], key : K) -> Option[V] {
    for i = 0; i < map.length(); i = i + 1 {
      let (k, v) = map[i]
      if (k == key) {
        return Some(v)
      }
    }
    None
  }
  
  pub fn insert[K : Eq, V](map : Map[K, V], key : K, value : V) -> Map[K, V] {
    let found = false
    let result = []
    
    for i = 0; i < map.length(); i = i + 1 {
      let (k, v) = map[i]
      if (k == key) {
        result.push((key, value))
        found = true
      } else {
        result.push((k, v))
      }
    }
    
    if (not(found)) {
      result.push((key, value))
    }
    
    result
  }
}

// 测试5: 栈操作
test "栈操作测试" {
  let empty_stack = []
  assert_eq(stack_is_empty(empty_stack), true)
  assert_eq(stack_size(empty_stack), 0)
  
  let stack1 = stack_push(empty_stack, 10)
  assert_eq(stack_is_empty(stack1), false)
  assert_eq(stack_size(stack1), 1)
  assert_eq(stack_peek(stack1), Some(10))
  
  let stack2 = stack_push(stack1, 20)
  assert_eq(stack_size(stack2), 2)
  assert_eq(stack_peek(stack2), Some(20))
  
  let (popped_value, stack3) = stack_pop(stack2)
  assert_eq(popped_value, Some(20))
  assert_eq(stack_size(stack3), 1)
  assert_eq(stack_peek(stack3), Some(10))
  
  let (popped_value2, stack4) = stack_pop(stack3)
  assert_eq(popped_value2, Some(10))
  assert_eq(stack_is_empty(stack4), true)
  
  let (popped_value3, stack5) = stack_pop(stack4)
  assert_eq(popped_value3, None)
  assert_eq(stack_is_empty(stack5), true)
}

fn stack_push[T](stack : Array[T], item : T) -> Array[T] {
  let result = stack.copy()
  result.push(item)
  result
}

fn stack_pop[T](stack : Array[T]) -> (Option[T], Array[T]) {
  if (stack.length() == 0) {
    (None, stack)
  } else {
    let result = stack.copy()
    let item = result.pop()
    (Some(item), result)
  }
}

fn stack_peek[T](stack : Array[T]) -> Option[T] {
  if (stack.length() == 0) {
    None
  } else {
    Some(stack[stack.length() - 1])
  }
}

fn stack_is_empty[T](stack : Array[T]) -> Bool {
  stack.length() == 0
}

fn stack_size[T](stack : Array[T]) -> Int {
  stack.length()
}