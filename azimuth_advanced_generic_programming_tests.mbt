// Azimuth Advanced Generic Programming Test Suite
// 高级泛型编程测试用例

// 测试1: 泛型容器实现
test "泛型容器实现测试" {
  // 泛型栈实现
  struct Stack[T] {
    items : Array[T]
  }
  
  fn Stack::new[T]() -> Stack[T] {
    Stack::{ items: [] }
  }
  
  fn Stack::push[T](stack : Stack[T], item : T) -> Stack[T] {
    Stack::{ items: stack.items.push(item) }
  }
  
  fn Stack::pop[T](stack : Stack[T]) -> (Stack[T], Option[T]) {
    match stack.items {
      [] => (stack, None)
      [head, ..tail] => (Stack::{ items: tail }, Some(head))
    }
  }
  
  fn Stack::peek[T](stack : Stack[T]) -> Option[T] {
    match stack.items {
      [] => None
      [head, ..] => Some(head)
    }
  }
  
  fn Stack::is_empty[T](stack : Stack[T]) -> Bool {
    stack.items.length() == 0
  }
  
  fn Stack::size[T](stack : Stack[T]) -> Int {
    stack.items.length()
  }
  
  // 泛型队列实现
  struct Queue[T] {
    enqueue : Array[T]
    dequeue : Array[T]
  }
  
  fn Queue::new[T]() -> Queue[T] {
    Queue::{ enqueue: [], dequeue: [] }
  }
  
  fn Queue::enqueue[T](queue : Queue[T], item : T) -> Queue[T] {
    Queue::{ enqueue: queue.enqueue.push(item), dequeue: queue.dequeue }
  }
  
  fn Queue::dequeue[T](queue : Queue[T]) -> (Queue[T], Option[T]) {
    if queue.dequeue.length() > 0 {
      match queue.dequeue {
        [] => (queue, None)  // 不可能发生
        [head, ..tail] => (Queue::{ enqueue: queue.enqueue, dequeue: tail }, Some(head))
      }
    } else if queue.enqueue.length() > 0 {
      // 反转enqueue数组到dequeue
      let reversed = queue.enqueue.reverse()
      match reversed {
        [] => (queue, None)  // 不可能发生
        [head, ..tail] => (Queue::{ enqueue: [], dequeue: tail }, Some(head))
      }
    } else {
      (queue, None)
    }
  }
  
  // 测试泛型栈
  let int_stack = Stack::new[Int]()
  let int_stack1 = Stack::push(int_stack, 1)
  let int_stack2 = Stack::push(int_stack1, 2)
  let int_stack3 = Stack::push(int_stack2, 3)
  
  assert_eq(Stack::size(int_stack3), 3)
  assert_eq(Stack::peek(int_stack3), Some(3))
  
  let (int_stack4, popped3) = Stack::pop(int_stack3)
  assert_eq(popped3, Some(3))
  assert_eq(Stack::size(int_stack4), 2)
  
  // 测试泛型字符串栈
  let string_stack = Stack::new[String]()
  let string_stack1 = Stack::push(string_stack, "hello")
  let string_stack2 = Stack::push(string_stack1, "world")
  
  assert_eq(Stack::size(string_stack2), 2)
  assert_eq(Stack::peek(string_stack2), Some("world"))
  
  // 测试泛型队列
  let int_queue = Queue::new[Int]()
  let int_queue1 = Queue::enqueue(int_queue, 1)
  let int_queue2 = Queue::enqueue(int_queue1, 2)
  let int_queue3 = Queue::enqueue(int_queue2, 3)
  
  let (int_queue4, dequeued1) = Queue::dequeue(int_queue3)
  assert_eq(dequeued1, Some(1))
  
  let (int_queue5, dequeued2) = Queue::dequeue(int_queue4)
  assert_eq(dequeued2, Some(2))
}

// 测试2: 泛型函数组合器
test "泛型函数组合器测试" {
  // 泛型映射函数
  fn map[T, U](array : Array[T], f : (T) -> U) -> Array[U] {
    let mut result = []
    for item in array {
      result = result.push(f(item))
    }
    result
  }
  
  // 泛型过滤函数
  fn filter[T](array : Array[T], predicate : (T) -> Bool) -> Array[T] {
    let mut result = []
    for item in array {
      if predicate(item) {
        result = result.push(item)
      }
    }
    result
  }
  
  // 泛型折叠函数
  fn fold[T, U](array : Array[T], initial : U, f : (U, T) -> U) -> U {
    let mut result = initial
    for item in array {
      result = f(result, item)
    }
    result
  }
  
  // 泛型函数组合
  fn compose[A, B, C](f : (B) -> C, g : (A) -> B) -> (A) -> C {
    fn(x : A) { f(g(x)) }
  }
  
  // 泛型管道操作
  fn pipe[T, U](value : T, f : (T) -> U) -> U {
    f(value)
  }
  
  // 泛型柯里化
  fn curry2[A, B, C](f : (A, B) -> C) -> (A) -> (B) -> C {
    fn(a : A) { fn(b : B) { f(a, b) } }
  }
  
  fn curry3[A, B, C, D](f : (A, B, C) -> D) -> (A) -> (B) -> (C) -> D {
    fn(a : A) { fn(b : B) { fn(c : C) { f(a, b, c) } } }
  }
  
  // 测试数据
  let numbers = [1, 2, 3, 4, 5]
  let strings = ["hello", "world", "azimuth", "telemetry"]
  
  // 测试映射
  let doubled = map(numbers, fn(x) { x * 2 })
  assert_eq(doubled, [2, 4, 6, 8, 10])
  
  let uppercased = map(strings, fn(s) { s.to_uppercase() })
  assert_eq(uppercased, ["HELLO", "WORLD", "AZIMUTH", "TELEMETRY"])
  
  // 测试过滤
  let evens = filter(numbers, fn(x) { x % 2 == 0 })
  assert_eq(evens, [2, 4])
  
  let long_strings = filter(strings, fn(s) { s.length() > 5 })
  assert_eq(long_strings, ["azimuth", "telemetry"])
  
  // 测试折叠
  let sum = fold(numbers, 0, fn(acc, x) { acc + x })
  assert_eq(sum, 15)
  
  let concatenated = fold(strings, "", fn(acc, s) { acc + s })
  assert_eq(concatenated, "helloworldazimuthtelemetry")
  
  // 测试函数组合
  let add_one = fn(x : Int) { x + 1 }
  let double = fn(x : Int) { x * 2 }
  let add_one_then_double = compose(double, add_one)
  
  assert_eq(add_one_then_double(5), 12)
  
  // 测试管道
  let result = pipe(5, add_one) |> double
  assert_eq(result, 12)
  
  // 测试柯里化
  let add = fn(a : Int, b : Int) { a + b }
  let curried_add = curry2(add)
  let add_5 = curried_add(5)
  
  assert_eq(add_5(3), 8)
  
  let multiply_three = fn(a : Int, b : Int, c : Int) { a * b * c }
  let curried_multiply = curry3(multiply_three)
  let multiply_by_2 = curried_multiply(2)
  let multiply_by_2_and_3 = multiply_by_2(3)
  
  assert_eq(multiply_by_2_and_3(4), 24)
}

// 测试3: 泛型模式匹配
test "泛型模式匹配测试" {
  // 泛型结果类型
  enum Result[T, E] {
    Ok(T)
    Err(E)
  }
  
  // 泛型可选类型
  enum Option[T] {
    Some(T)
    None
  }
  
  // 泛型二叉树
  enum BinaryTree[T] {
    Leaf(T)
    Node(BinaryTree[T], BinaryTree[T])
  }
  
  // 泛型模式匹配函数
  fn map_result[T, E, U](result : Result[T, E], f : (T) -> U) -> Result[U, E] {
    match result {
      Result::Ok(value) => Result::Ok(f(value))
      Result::Err(error) => Result::Err(error)
    }
  }
  
  fn map_option[T, U](option : Option[T], f : (T) -> U) -> Option[U] {
    match option {
      Option::Some(value) => Option::Some(f(value))
      Option::None => Option::None
    }
  }
  
  fn tree_map[T, U](tree : BinaryTree[T], f : (T) -> U) -> BinaryTree[U] {
    match tree {
      BinaryTree::Leaf(value) => BinaryTree::Leaf(f(value))
      BinaryTree::Node(left, right) => BinaryTree::Node(tree_map(left, f), tree_map(right, f))
    }
  }
  
  fn tree_fold[T, U](tree : BinaryTree[T], initial : U, f : (U, T) -> U) -> U {
    match tree {
      BinaryTree::Leaf(value) => f(initial, value)
      BinaryTree::Node(left, right) => {
        let left_result = tree_fold(left, initial, f)
        let right_result = tree_fold(right, left_result, f)
        right_result
      }
    }
  }
  
  // 测试结果类型
  let success = Result::Ok(42)
  let failure = Result::Err("error message")
  
  let mapped_success = map_result(success, fn(x) { x * 2 })
  match mapped_success {
    Result::Ok(value) => assert_eq(value, 84)
    Result::Err(_) => assert_true(false)
  }
  
  let mapped_failure = map_result(failure, fn(x) { x * 2 })
  match mapped_failure {
    Result::Ok(_) => assert_true(false)
    Result::Err(error) => assert_eq(error, "error message")
  }
  
  // 测试可选类型
  let some_value = Option::Some("hello")
  let none_value = Option::None
  
  let mapped_some = map_option(some_value, fn(s) { s.to_uppercase() })
  match mapped_some {
    Option::Some(value) => assert_eq(value, "HELLO")
    Option::None => assert_true(false)
  }
  
  let mapped_none = map_option(none_value, fn(s) { s.to_uppercase() })
  match mapped_none {
    Option::Some(_) => assert_true(false)
    Option::None => assert_true(true)
  }
  
  // 测试二叉树
  let tree = BinaryTree::Node(
    BinaryTree::Leaf(1),
    BinaryTree::Node(
      BinaryTree::Leaf(2),
      BinaryTree::Leaf(3)
    )
  )
  
  let mapped_tree = tree_map(tree, fn(x) { x * 2 })
  let sum = tree_fold(mapped_tree, 0, fn(acc, x) { acc + x })
  
  assert_eq(sum, 12)  // (1*2) + (2*2) + (3*2) = 2 + 4 + 6 = 12
}

// 测试4: 泛型约束和类型类
test "泛型约束和类型类测试" {
  // 定义可比较约束
  trait Comparable[T] {
    compare(T, T) -> Int  // 返回-1, 0, 1
  }
  
  // 为Int实现Comparable
  impl Comparable[Int] for Int {
    fn compare(a : Int, b : Int) -> Int {
      if a < b { -1 } else if a > b { 1 } else { 0 }
    }
  }
  
  // 为String实现Comparable
  impl Comparable[String] for String {
    fn compare(a : String, b : String) -> Int {
      if a < b { -1 } else if a > b { 1 } else { 0 }
    }
  }
  
  // 定义可加约束
  trait Addable[T] {
    add(T, T) -> T
  }
  
  // 为Int实现Addable
  impl Addable[Int] for Int {
    fn add(a : Int, b : Int) -> Int { a + b }
  }
  
  // 为String实现Addable
  impl Addable[String] for String {
    fn add(a : String, b : String) -> String { a + b }
  }
  
  // 泛型最大值函数
  fn max[T : Comparable](a : T, b : T) -> T {
    if Comparable::compare(a, b) >= 0 { a } else { b }
  }
  
  // 泛型求和函数
  fn sum[T : Addable](array : Array[T]) -> T {
    match array {
      [] => panic("Cannot sum empty array")
      [head, ..tail] => {
        let mut result = head
        for item in tail {
          result = Addable::add(result, item)
        }
        result
      }
    }
  }
  
  // 泛型排序函数
  fn sort[T : Comparable](array : Array[T]) -> Array[T] {
    fn insertion_sort[T : Comparable](array : Array[T]) -> Array[T] {
      let mut result = array
      for i in 1..result.length() {
        let key = result[i]
        let mut j = i - 1
        while j >= 0 && Comparable::compare(result[j], key) > 0 {
          result[j + 1] = result[j]
          j = j - 1
        }
        result[j + 1] = key
      }
      result
    }
    insertion_sort(array)
  }
  
  // 测试可比较约束
  assert_eq(max(5, 3), 5)
  assert_eq(max(2, 7), 7)
  assert_eq(max("hello", "world"), "world")
  assert_eq(max("azimuth", "telemetry"), "telemetry")
  
  // 测试可加约束
  let int_numbers = [1, 2, 3, 4, 5]
  assert_eq(sum(int_numbers), 15)
  
  let strings = ["a", "b", "c"]
  assert_eq(sum(strings), "abc")
  
  // 测试泛型排序
  let unsorted_ints = [5, 2, 8, 1, 9]
  let sorted_ints = sort(unsorted_ints)
  assert_eq(sorted_ints, [1, 2, 5, 8, 9])
  
  let unsorted_strings = ["zebra", "apple", "orange", "banana"]
  let sorted_strings = sort(unsorted_strings)
  assert_eq(sorted_strings, ["apple", "banana", "orange", "zebra"])
}

// 测试5: 高阶泛型函数
test "高阶泛型函数测试" {
  // 泛型谓词组合
  fn and[T](p1 : (T) -> Bool, p2 : (T) -> Bool) -> (T) -> Bool {
    fn(x : T) { p1(x) && p2(x) }
  }
  
  fn or[T](p1 : (T) -> Bool, p2 : (T) -> Bool) -> (T) -> Bool {
    fn(x : T) { p1(x) || p2(x) }
  }
  
  fn not[T](p : (T) -> Bool) -> (T) -> Bool {
    fn(x : T) { !p(x) }
  }
  
  // 泛型转换器组合
  fn compose_transform[A, B, C](f1 : (A) -> B, f2 : (B) -> C) -> (A) -> C {
    fn(x : A) { f2(f1(x)) }
  }
  
  // 泛型迭代器
  struct Iterator[T] {
    items : Array[T]
    index : Int
  }
  
  fn Iterator::new[T](items : Array[T]) -> Iterator[T] {
    Iterator::{ items, index: 0 }
  }
  
  fn Iterator::next[T](iterator : Iterator[T]) -> (Iterator[T], Option[T]) {
    if iterator.index >= iterator.items.length() {
      (iterator, None)
    } else {
      let item = iterator.items[iterator.index]
      (Iterator::{ items: iterator.items, index: iterator.index + 1 }, Some(item))
    }
  }
  
  fn Iterator::map[T, U](iterator : Iterator[T], f : (T) -> U) -> Iterator[U] {
    fn map_helper[T, U](iterator : Iterator[T], f : (T) -> U, result : Array[U]) -> Array[U] {
      match Iterator::next(iterator) {
        (_, None) => result
        (new_iterator, Some(item)) => {
          map_helper(new_iterator, f, result.push(f(item)))
        }
      }
    }
    Iterator::new(map_helper(iterator, f, []))
  }
  
  fn Iterator::filter[T](iterator : Iterator[T], predicate : (T) -> Bool) -> Iterator[T] {
    fn filter_helper[T](iterator : Iterator[T], predicate : (T) -> Bool, result : Array[T]) -> Array[T] {
      match Iterator::next(iterator) {
        (_, None) => result
        (new_iterator, Some(item)) => {
          if predicate(item) {
            filter_helper(new_iterator, predicate, result.push(item))
          } else {
            filter_helper(new_iterator, predicate, result)
          }
        }
      }
    }
    Iterator::new(filter_helper(iterator, predicate, []))
  }
  
  fn Iterator::collect[T, U](iterator : Iterator[U]) -> Array[U] {
    iterator.items
  }
  
  // 测试谓词组合
  let is_even = fn(x : Int) { x % 2 == 0 }
  let is_positive = fn(x : Int) { x > 0 }
  let is_even_and_positive = and(is_even, is_positive)
  let is_even_or_positive = or(is_even, is_positive)
  let is_not_even = not(is_even)
  
  assert_true(is_even_and_positive(4))
  assert_false(is_even_and_positive(3))
  assert_false(is_even_and_positive(-2))
  
  assert_true(is_even_or_positive(4))
  assert_true(is_even_or_positive(3))
  assert_false(is_even_or_positive(-1))
  
  assert_true(is_not_even(3))
  assert_false(is_not_even(4))
  
  // 测试转换器组合
  let to_string = fn(x : Int) { x.to_string() }
  let add_prefix = fn(s : String) { "num_" + s }
  let int_to_prefixed_string = compose_transform(to_string, add_prefix)
  
  assert_eq(int_to_prefixed_string(42), "num_42")
  
  // 测试泛型迭代器
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let iterator = Iterator::new(numbers)
  
  let even_numbers = Iterator::filter(iterator, is_even)
  let doubled_evens = Iterator::map(even_numbers, fn(x) { x * 2 })
  let result = Iterator::collect(doubled_evens)
  
  assert_eq(result, [4, 8, 12, 16, 20])
}