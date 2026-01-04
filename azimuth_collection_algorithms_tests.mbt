// Azimuth 集合操作和算法测试用例
// 专注于数据结构操作、算法实现和集合处理功能测试

// 测试1: 数组基础操作扩展
test "数组基础操作扩展测试" {
  // 测试数组创建和初始化
  let empty_array = []
  assert_eq(empty_array.length(), 0)
  
  let filled_array = [1, 2, 3, 4, 5]
  assert_eq(filled_array.length(), 5)
  
  // 测试数组访问和修改
  let mut mutable_array = [10, 20, 30]
  assert_eq(mutable_array[0], 10)
  mutable_array[0] = 15
  assert_eq(mutable_array[0], 15)
  
  // 测试数组添加和删除元素
  let mut dynamic_array = [1, 2, 3]
  dynamic_array = dynamic_array.push(4)
  assert_eq(dynamic_array.length(), 4)
  assert_eq(dynamic_array[3], 4)
  
  dynamic_array = dynamic_array.pop()
  assert_eq(dynamic_array.length(), 3)
  
  // 测试数组切片
  let original = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let slice = original.slice(2, 5) // 包含索引2到4的元素
  assert_eq(slice.length(), 3)
  assert_eq(slice[0], 3)
  assert_eq(slice[2], 5)
}

// 测试2: 数组高级操作
test "数组高级操作测试" {
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  // 测试数组过滤
  let even_numbers = numbers.filter(fn(x) { x % 2 == 0 })
  assert_eq(even_numbers.length(), 5)
  assert_eq(even_numbers[0], 2)
  assert_eq(even_numbers[4], 10)
  
  let greater_than_five = numbers.filter(fn(x) { x > 5 })
  assert_eq(greater_than_five.length(), 5)
  assert_eq(greater_than_five[0], 6)
  
  // 测试数组映射
  let squared = numbers.map(fn(x) { x * x })
  assert_eq(squared.length(), 10)
  assert_eq(squared[0], 1)
  assert_eq(squared[9], 100)
  
  let doubled = numbers.map(fn(x) { x * 2 })
  assert_eq(doubled[4], 10)
  
  // 测试数组归约
  let sum = numbers.reduce(fn(acc, x) { acc + x }, 0)
  assert_eq(sum, 55)
  
  let product = numbers.reduce(fn(acc, x) { acc * x }, 1)
  assert_eq(product, 3628800)
  
  let max_value = numbers.reduce(fn(acc, x) { if x > acc { x } else { acc } }, numbers[0])
  assert_eq(max_value, 10)
}

// 测试3: 数组查找和排序
test "数组查找和排序测试" {
  let unsorted = [5, 2, 8, 1, 9, 3, 7, 4, 6, 10]
  
  // 测试数组查找
  assert_eq(unsorted.find(fn(x) { x == 7 }), Some(6)) // 索引6
  assert_eq(unsorted.find(fn(x) { x == 11 }), None)
  
  let first_even = unsorted.find(fn(x) { x % 2 == 0 })
  assert_eq(first_even, Some(1)) // 索引1，值为2
  
  // 测试数组包含检查
  assert_true(unsorted.contains(5))
  assert_true(unsorted.contains(10))
  assert_false(unsorted.contains(11))
  
  // 测试数组排序
  let sorted = unsorted.sort(fn(a, b) { a - b })
  assert_eq(sorted[0], 1)
  assert_eq(sorted[9], 10)
  
  let reverse_sorted = unsorted.sort(fn(a, b) { b - a })
  assert_eq(reverse_sorted[0], 10)
  assert_eq(reverse_sorted[9], 1)
  
  // 测试数组去重
  let with_duplicates = [1, 2, 2, 3, 3, 3, 4, 5, 5]
  let unique = with_duplicates.unique()
  assert_eq(unique.length(), 5)
  assert_eq(unique[0], 1)
  assert_eq(unique[4], 5)
}

// 测试4: 列表（链表）操作
test "列表操作测试" {
  // 创建链表
  let empty_list = List::empty()
  assert_true(List::is_empty(empty_list))
  
  let list1 = List::cons(1, List::cons(2, List::cons(3, List::empty())))
  assert_eq(List::length(list1), 3)
  
  // 测试链表头部和尾部操作
  let head = List::head(list1)
  assert_eq(head, 1)
  
  let tail = List::tail(list1)
  assert_eq(List::length(tail), 2)
  assert_eq(List::head(tail), 2)
  
  // 测试链表添加元素
  let list2 = List::append(list1, 4)
  assert_eq(List::length(list2), 4)
  
  let list3 = List::prepend(list1, 0)
  assert_eq(List::head(list3), 0)
  assert_eq(List::length(list3), 4)
  
  // 测试链表反转
  let reversed = List::reverse(list1)
  assert_eq(List::head(reversed), 3)
  assert_eq(List::head(List::tail(reversed)), 2)
}

// 测试5: 集合（Set）操作
test "集合操作测试" {
  // 创建集合
  let empty_set = Set::empty()
  assert_eq(Set::size(empty_set), 0)
  
  let set1 = Set::from_array([1, 2, 3, 4, 5])
  assert_eq(Set::size(set1), 5)
  
  // 测试集合添加和删除
  let set2 = Set::add(set1, 6)
  assert_eq(Set::size(set2), 6)
  assert_true(Set::contains(set2, 6))
  
  let set3 = Set::remove(set2, 3)
  assert_eq(Set::size(set3), 5)
  assert_false(Set::contains(set3, 3))
  
  // 测试集合包含检查
  assert_true(Set::contains(set1, 3))
  assert_false(Set::contains(set1, 6))
  
  // 测试集合运算
  let set_a = Set::from_array([1, 2, 3, 4, 5])
  let set_b = Set::from_array([4, 5, 6, 7, 8])
  
  let union_set = Set::union(set_a, set_b)
  assert_eq(Set::size(union_set), 8)
  assert_true(Set::contains(union_set, 1))
  assert_true(Set::contains(union_set, 8))
  
  let intersection_set = Set::intersection(set_a, set_b)
  assert_eq(Set::size(intersection_set), 2)
  assert_true(Set::contains(intersection_set, 4))
  assert_true(Set::contains(intersection_set, 5))
  
  let difference_set = Set::difference(set_a, set_b)
  assert_eq(Set::size(difference_set), 3)
  assert_true(Set::contains(difference_set, 1))
  assert_false(Set::contains(difference_set, 4))
}

// 测试6: 字典（Map）操作
test "字典操作测试" {
  // 创建字典
  let empty_map = Map::empty()
  assert_eq(Map::size(empty_map), 0)
  
  let map1 = Map::from_array([("a", 1), ("b", 2), ("c", 3)])
  assert_eq(Map::size(map1), 3)
  
  // 测试字典添加和更新
  let map2 = Map::set(map1, "d", 4)
  assert_eq(Map::size(map2), 4)
  assert_eq(Map::get(map2, "d"), Some(4))
  
  let map3 = Map::set(map2, "b", 20) // 更新现有键
  assert_eq(Map::size(map3), 4)
  assert_eq(Map::get(map3, "b"), Some(20))
  
  // 测试字典查找
  assert_eq(Map::get(map1, "a"), Some(1))
  assert_eq(Map::get(map1, "d"), None)
  
  let value_with_default = Map::get_with_default(map1, "z", 0)
  assert_eq(value_with_default, 0)
  
  // 测试字典删除
  let map4 = Map::remove(map3, "c")
  assert_eq(Map::size(map4), 3)
  assert_eq(Map::get(map4, "c"), None)
  
  // 测试字典键和值
  let keys = Map::keys(map1)
  assert_eq(keys.length(), 3)
  
  let values = Map::values(map1)
  assert_eq(values.length(), 3)
  
  let contains_key = Map::contains_key(map1, "b")
  assert_true(contains_key)
}

// 测试7: 搜索算法
test "搜索算法测试" {
  let sorted_array = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
  let unsorted_array = [5, 2, 8, 1, 9, 3, 7, 4, 6, 10]
  
  // 测试线性搜索
  let linear_result = linear_search(unsorted_array, 7)
  assert_eq(linear_result, Some(6)) // 索引6
  
  let linear_not_found = linear_search(unsorted_array, 11)
  assert_eq(linear_not_found, None)
  
  // 测试二分搜索
  let binary_result = binary_search(sorted_array, 7)
  assert_eq(binary_result, Some(3)) // 索引3
  
  let binary_not_found = binary_search(sorted_array, 8)
  assert_eq(binary_not_found, None)
  
  // 测试查找第一个和最后一个匹配元素
  let with_duplicates = [1, 2, 2, 2, 3, 4, 5]
  let first_match = find_first(with_duplicates, 2)
  assert_eq(first_match, Some(1))
  
  let last_match = find_last(with_duplicates, 2)
  assert_eq(last_match, Some(3))
  
  // 测试查找所有匹配元素
  let all_matches = find_all(with_duplicates, 2)
  assert_eq(all_matches.length(), 3)
  assert_eq(all_matches[0], 1)
  assert_eq(all_matches[1], 2)
  assert_eq(all_matches[2], 3)
}

// 测试8: 排序算法
test "排序算法测试" {
  let unsorted = [5, 2, 8, 1, 9, 3, 7, 4, 6, 10]
  
  // 测试冒泡排序
  let bubble_sorted = bubble_sort(unsorted)
  assert_eq(bubble_sorted[0], 1)
  assert_eq(bubble_sorted[9], 10)
  
  // 测试选择排序
  let selection_sorted = selection_sort(unsorted)
  assert_eq(selection_sorted[0], 1)
  assert_eq(selection_sorted[9], 10)
  
  // 测试插入排序
  let insertion_sorted = insertion_sort(unsorted)
  assert_eq(insertion_sorted[0], 1)
  assert_eq(insertion_sorted[9], 10)
  
  // 测试快速排序
  let quick_sorted = quick_sort(unsorted)
  assert_eq(quick_sorted[0], 1)
  assert_eq(quick_sorted[9], 10)
  
  // 测试归并排序
  let merge_sorted = merge_sort(unsorted)
  assert_eq(merge_sorted[0], 1)
  assert_eq(merge_sorted[9], 10)
  
  // 测试自定义比较器排序
  let reverse_sorted = quick_sort_custom(unsorted, fn(a, b) { b - a })
  assert_eq(reverse_sorted[0], 10)
  assert_eq(reverse_sorted[9], 1)
}

// 测试9: 栈和队列操作
test "栈和队列操作测试" {
  // 测试栈操作
  let empty_stack = Stack::empty()
  assert_true(Stack::is_empty(empty_stack))
  
  let stack1 = Stack::push(empty_stack, 1)
  let stack2 = Stack::push(stack1, 2)
  let stack3 = Stack::push(stack2, 3)
  
  assert_eq(Stack::size(stack3), 3)
  
  let (top, stack4) = Stack::pop(stack3)
  assert_eq(top, 3)
  assert_eq(Stack::size(stack4), 2)
  
  // 测试队列操作
  let empty_queue = Queue::empty()
  assert_true(Queue::is_empty(empty_queue))
  
  let queue1 = Queue::enqueue(empty_queue, 1)
  let queue2 = Queue::enqueue(queue1, 2)
  let queue3 = Queue::enqueue(queue2, 3)
  
  assert_eq(Queue::size(queue3), 3)
  
  let (front, queue4) = Queue::dequeue(queue3)
  assert_eq(front, 1)
  assert_eq(Queue::size(queue4), 2)
  
  let (front2, queue5) = Queue::dequeue(queue4)
  assert_eq(front2, 2)
  assert_eq(Queue::size(queue5), 1)
}

// 测试10: 树结构操作
test "树结构操作测试" {
  // 创建二叉搜索树
  let empty_tree = BinarySearchTree::empty()
  assert_true(BinarySearchTree::is_empty(empty_tree))
  
  let tree1 = BinarySearchTree::insert(empty_tree, 5)
  let tree2 = BinarySearchTree::insert(tree1, 3)
  let tree3 = BinarySearchTree::insert(tree2, 7)
  let tree4 = BinarySearchTree::insert(tree3, 2)
  let tree5 = BinarySearchTree::insert(tree4, 4)
  let tree6 = BinarySearchTree::insert(tree5, 6)
  let tree7 = BinarySearchTree::insert(tree6, 8)
  
  // 测试树查找
  assert_true(BinarySearchTree::contains(tree7, 5))
  assert_true(BinarySearchTree::contains(tree7, 2))
  assert_true(BinarySearchTree::contains(tree7, 8))
  assert_false(BinarySearchTree::contains(tree7, 9))
  
  // 测试树遍历
  let inorder = BinarySearchTree::inorder_traversal(tree7)
  assert_eq(inorder, [2, 3, 4, 5, 6, 7, 8])
  
  let preorder = BinarySearchTree::preorder_traversal(tree7)
  assert_eq(preorder[0], 5) // 根节点
  
  let postorder = BinarySearchTree::postorder_traversal(tree7)
  assert_eq(postorder[6], 5) // 根节点在最后
  
  // 测试树删除
  let tree8 = BinarySearchTree::remove(tree7, 3)
  assert_false(BinarySearchTree::contains(tree8, 3))
  assert_true(BinarySearchTree::contains(tree8, 2))
  assert_true(BinarySearchTree::contains(tree8, 4))
  
  // 测试树大小和高度
  assert_eq(BinarySearchTree::size(tree7), 7)
  assert_eq(BinarySearchTree::height(tree7), 2)
}

// 辅助函数定义（实际实现中这些函数应该来自标准库或实现模块）
fn linear_search[T](array : Array[T], target : T) -> Option[Int] {
  for i in 0..array.length() {
    if array[i] == target {
      return Some(i)
    }
  }
  None
}

fn binary_search[T](array : Array[T], target : T) -> Option[Int] {
  let mut left = 0
  let mut right = array.length() - 1
  
  while left <= right {
    let mid = left + (right - left) / 2
    if array[mid] == target {
      return Some(mid)
    } else if array[mid] < target {
      left = mid + 1
    } else {
      right = mid - 1
    }
  }
  
  None
}

fn find_first[T](array : Array[T], target : T) -> Option[Int] {
  for i in 0..array.length() {
    if array[i] == target {
      return Some(i)
    }
  }
  None
}

fn find_last[T](array : Array[T], target : T) -> Option[Int] {
  let mut result = None
  for i in 0..array.length() {
    if array[i] == target {
      result = Some(i)
    }
  }
  result
}

fn find_all[T](array : Array[T], target : T) -> Array[Int] {
  let mut result = []
  for i in 0..array.length() {
    if array[i] == target {
      result = result.push(i)
    }
  }
  result
}

fn bubble_sort(array : Array[Int]) -> Array[Int] {
  let mut result = array.clone()
  let n = result.length()
  
  for i in 0..n {
    for j in 0..(n - i - 1) {
      if result[j] > result[j + 1] {
        let temp = result[j]
        result[j] = result[j + 1]
        result[j + 1] = temp
      }
    }
  }
  
  result
}

fn selection_sort(array : Array[Int]) -> Array[Int] {
  let mut result = array.clone()
  let n = result.length()
  
  for i in 0..n {
    let mut min_index = i
    for j in (i + 1)..n {
      if result[j] < result[min_index] {
        min_index = j
      }
    }
    
    if min_index != i {
      let temp = result[i]
      result[i] = result[min_index]
      result[min_index] = temp
    }
  }
  
  result
}

fn insertion_sort(array : Array[Int]) -> Array[Int] {
  let mut result = array.clone()
  let n = result.length()
  
  for i in 1..n {
    let key = result[i]
    let mut j = i - 1
    
    while j >= 0 && result[j] > key {
      result[j + 1] = result[j]
      j = j - 1
    }
    
    result[j + 1] = key
  }
  
  result
}

fn quick_sort(array : Array[Int]) -> Array[Int] {
  if array.length() <= 1 {
    return array
  }
  
  let pivot = array[0]
  let less = array.filter(fn(x) { x < pivot })
  let equal = array.filter(fn(x) { x == pivot })
  let greater = array.filter(fn(x) { x > pivot })
  
  quick_sort(less) + equal + quick_sort(greater)
}

fn quick_sort_custom(array : Array[Int], compare : (Int, Int) -> Int) -> Array[Int] {
  if array.length() <= 1 {
    return array
  }
  
  let pivot = array[0]
  let less = array.filter(fn(x) { compare(x, pivot) < 0 })
  let equal = array.filter(fn(x) { compare(x, pivot) == 0 })
  let greater = array.filter(fn(x) { compare(x, pivot) > 0 })
  
  quick_sort_custom(less, compare) + equal + quick_sort_custom(greater, compare)
}

fn merge_sort(array : Array[Int]) -> Array[Int] {
  if array.length() <= 1 {
    return array
  }
  
  let mid = array.length() / 2
  let left = merge_sort(array.slice(0, mid))
  let right = merge_sort(array.slice(mid, array.length()))
  
  merge(left, right)
}

fn merge(left : Array[Int], right : Array[Int]) -> Array[Int] {
  let mut result = []
  let mut i = 0
  let mut j = 0
  
  while i < left.length() && j < right.length() {
    if left[i] <= right[j] {
      result = result.push(left[i])
      i = i + 1
    } else {
      result = result.push(right[j])
      j = j + 1
    }
  }
  
  while i < left.length() {
    result = result.push(left[i])
    i = i + 1
  }
  
  while j < right.length() {
    result = result.push(right[j])
    j = j + 1
  }
  
  result
}

// 简化的数据结构定义（实际实现中这些应该有完整的实现）
type List[T] {
  // 链表实现
}

type Set[T] {
  // 集合实现
}

type Map[K, V] {
  // 字典实现
}

type Stack[T] {
  // 栈实现
}

type Queue[T] {
  // 队列实现
}

type BinarySearchTree[T] {
  // 二叉搜索树实现
}

// 简化的操作函数（实际实现中这些应该有完整的实现）
fn List::empty[T]() -> List[T] { /* 实现 */ }
fn List::is_empty[T](list : List[T]) -> Bool { /* 实现 */ }
fn List::cons[T](head : T, tail : List[T]) -> List[T] { /* 实现 */ }
fn List::length[T](list : List[T]) -> Int { /* 实现 */ }
fn List::head[T](list : List[T]) -> T { /* 实现 */ }
fn List::tail[T](list : List[T]) -> List[T] { /* 实现 */ }
fn List::append[T](list : List[T], value : T) -> List[T] { /* 实现 */ }
fn List::prepend[T](list : List[T], value : T) -> List[T] { /* 实现 */ }
fn List::reverse[T](list : List[T]) -> List[T] { /* 实现 */ }

fn Set::empty[T]() -> Set[T] { /* 实现 */ }
fn Set::size[T](set : Set[T]) -> Int { /* 实现 */ }
fn Set::from_array[T](array : Array[T]) -> Set[T] { /* 实现 */ }
fn Set::add[T](set : Set[T], value : T) -> Set[T] { /* 实现 */ }
fn Set::remove[T](set : Set[T], value : T) -> Set[T] { /* 实现 */ }
fn Set::contains[T](set : Set[T], value : T) -> Bool { /* 实现 */ }
fn Set::union[T](set1 : Set[T], set2 : Set[T]) -> Set[T] { /* 实现 */ }
fn Set::intersection[T](set1 : Set[T], set2 : Set[T]) -> Set[T] { /* 实现 */ }
fn Set::difference[T](set1 : Set[T], set2 : Set[T]) -> Set[T] { /* 实现 */ }

fn Map::empty[K, V]() -> Map[K, V] { /* 实现 */ }
fn Map::size[K, V](map : Map[K, V]) -> Int { /* 实现 */ }
fn Map::from_array[K, V](array : Array[(K, V)]) -> Map[K, V] { /* 实现 */ }
fn Map::set[K, V](map : Map[K, V], key : K, value : V) -> Map[K, V] { /* 实现 */ }
fn Map::get[K, V](map : Map[K, V], key : K) -> Option[V] { /* 实现 */ }
fn Map::get_with_default[K, V](map : Map[K, V], key : K, default : V) -> V { /* 实现 */ }
fn Map::remove[K, V](map : Map[K, V], key : K) -> Map[K, V] { /* 实现 */ }
fn Map::keys[K, V](map : Map[K, V]) -> Array[K] { /* 实现 */ }
fn Map::values[K, V](map : Map[K, V]) -> Array[V] { /* 实现 */ }
fn Map::contains_key[K, V](map : Map[K, V], key : K) -> Bool { /* 实现 */ }

fn Stack::empty[T]() -> Stack[T] { /* 实现 */ }
fn Stack::is_empty[T](stack : Stack[T]) -> Bool { /* 实现 */ }
fn Stack::push[T](stack : Stack[T], value : T) -> Stack[T] { /* 实现 */ }
fn Stack::pop[T](stack : Stack[T]) -> (T, Stack[T]) { /* 实现 */ }
fn Stack::size[T](stack : Stack[T]) -> Int { /* 实现 */ }

fn Queue::empty[T]() -> Queue[T] { /* 实现 */ }
fn Queue::is_empty[T](queue : Queue[T]) -> Bool { /* 实现 */ }
fn Queue::enqueue[T](queue : Queue[T], value : T) -> Queue[T] { /* 实现 */ }
fn Queue::dequeue[T](queue : Queue[T]) -> (T, Queue[T]) { /* 实现 */ }
fn Queue::size[T](queue : Queue[T]) -> Int { /* 实现 */ }

fn BinarySearchTree::empty[T]() -> BinarySearchTree[T] { /* 实现 */ }
fn BinarySearchTree::is_empty[T](tree : BinarySearchTree[T]) -> Bool { /* 实现 */ }
fn BinarySearchTree::insert[T](tree : BinarySearchTree[T], value : T) -> BinarySearchTree[T] { /* 实现 */ }
fn BinarySearchTree::contains[T](tree : BinarySearchTree[T], value : T) -> Bool { /* 实现 */ }
fn BinarySearchTree::remove[T](tree : BinarySearchTree[T], value : T) -> BinarySearchTree[T] { /* 实现 */ }
fn BinarySearchTree::size[T](tree : BinarySearchTree[T]) -> Int { /* 实现 */ }
fn BinarySearchTree::height[T](tree : BinarySearchTree[T]) -> Int { /* 实现 */ }
fn BinarySearchTree::inorder_traversal[T](tree : BinarySearchTree[T]) -> Array[T] { /* 实现 */ }
fn BinarySearchTree::preorder_traversal[T](tree : BinarySearchTree[T]) -> Array[T] { /* 实现 */ }
fn BinarySearchTree::postorder_traversal[T](tree : BinarySearchTree[T]) -> Array[T] { /* 实现 */ }