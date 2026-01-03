// Azimuth Type System and Functional Programming Tests
// 类型系统和函数式编程测试用例，专注于类型安全、函数式编程范式、高阶函数和不可变性

// Test 1: 类型安全和类型推导测试
test "类型安全和类型推导测试" {
  // 基本类型操作
  let int_value : Int = 42
  let float_value : Float = 3.14
  let string_value : String = "Hello, MoonBit!"
  let bool_value : Bool = true
  
  // 验证类型
  assert_eq(int_value, 42)
  assert_eq(float_value, 3.14)
  assert_eq(string_value, "Hello, MoonBit!")
  assert_true(bool_value)
  
  // 类型转换
  let int_to_float : Float = int_value.to_float()
  let float_to_int : Int = float_value.to_int()
  
  // 验证类型转换
  assert_eq(int_to_float, 42.0)
  assert_eq(float_to_int, 3)
  
  // 可选类型处理
  let some_value : Option[Int] = Some(42)
  let none_value : Option[Int] = None
  
  // 验证可选类型
  match some_value {
    Some(v) => assert_eq(v, 42)
    None => assert_true(false)
  }
  
  match none_value {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 数组类型操作
  let int_array : Array[Int] = [1, 2, 3, 4, 5]
  let string_array : Array[String] = ["a", "b", "c"]
  
  // 验证数组类型
  assert_eq(int_array.length(), 5)
  assert_eq(int_array[0], 1)
  assert_eq(string_array.length(), 3)
  assert_eq(string_array[2], "c")
  
  // 元组类型操作
  let tuple : (Int, String, Bool) = (42, "answer", true)
  
  // 验证元组类型
  assert_eq(tuple.0, 42)
  assert_eq(tuple.1, "answer")
  assert_true(tuple.2)
}

// Test 2: 高阶函数测试
test "高阶函数测试" {
  // 定义一个简单的函数
  fn add_five(x : Int) : Int {
    x + 5
  }
  
  fn multiply_by_two(x : Int) : Int {
    x * 2
  }
  
  // 高阶函数：接受函数作为参数
  fn apply_function(arr : Array[Int], func : (Int) -> Int) : Array[Int] {
    let mut result = []
    for item in arr {
      result = result + [func(item)]
    }
    result
  }
  
  // 测试数据
  let numbers = [1, 2, 3, 4, 5]
  
  // 应用高阶函数
  let added_five = apply_function(numbers, add_five)
  let multiplied = apply_function(numbers, multiply_by_two)
  
  // 验证结果
  assert_eq(added_five, [6, 7, 8, 9, 10])
  assert_eq(multiplied, [2, 4, 6, 8, 10])
  
  // 高阶函数：返回函数
  fn create_multiplier(factor : Int) : (Int) -> Int {
    fn(x : Int) : Int {
      x * factor
    }
  }
  
  // 创建特定的乘法函数
  let triple = create_multiplier(3)
  let quadruple = create_multiplier(4)
  
  // 验证返回的函数
  assert_eq(triple(5), 15)
  assert_eq(quadruple(5), 20)
  
  // 高阶函数：函数组合
  fn compose(f : (Int) -> Int, g : (Int) -> Int) : (Int) -> Int {
    fn(x : Int) : Int {
      f(g(x))
    }
  }
  
  // 组合函数
  let add_then_multiply = compose(multiply_by_two, add_five)
  
  // 验证函数组合
  assert_eq(add_then_multiply(10), 30) // (10 + 5) * 2 = 30
}

// Test 3: 不可变性和纯函数测试
test "不可变性和纯函数测试" {
  // 纯函数：相同输入总是产生相同输出，无副作用
  fn pure_add(x : Int, y : Int) : Int {
    x + y
  }
  
  // 验证纯函数特性
  assert_eq(pure_add(2, 3), 5)
  assert_eq(pure_add(2, 3), 5) // 多次调用结果相同
  assert_eq(pure_add(3, 2), 5)
  
  // 不可变数据操作
  let original_list = [1, 2, 3, 4, 5]
  
  // 不修改原列表，返回新列表
  fn add_to_list(list : Array[Int], item : Int) : Array[Int] {
    list + [item]
  }
  
  fn remove_from_list(list : Array[Int], index : Int) : Array[Int] {
    let mut result = []
    for i in 0 ..< list.length() {
      if i != index {
        result = result + [list[i]]
      }
    }
    result
  }
  
  // 应用不可变操作
  let with_new_item = add_to_list(original_list, 6)
  let without_third = remove_from_list(original_list, 2)
  
  // 验证不可变性
  assert_eq(original_list, [1, 2, 3, 4, 5]) // 原列表未改变
  assert_eq(with_new_item, [1, 2, 3, 4, 5, 6]) // 新列表包含新项
  assert_eq(without_third, [1, 2, 4, 5]) // 新列表不包含第三项
  
  // 纯函数式映射
  fn pure_map(list : Array[Int], func : (Int) -> Int) : Array[Int] {
    let mut result = []
    for item in list {
      result = result + [func(item)]
    }
    result
  }
  
  // 应用纯函数式映射
  let doubled = pure_map(original_list, fn(x) { x * 2 })
  let squared = pure_map(original_list, fn(x) { x * x })
  
  // 验证映射结果
  assert_eq(doubled, [2, 4, 6, 8, 10])
  assert_eq(squared, [1, 4, 9, 16, 25])
  
  // 验证原列表未改变
  assert_eq(original_list, [1, 2, 3, 4, 5])
}

// Test 4: 函数式数据结构测试
test "函数式数据结构测试" {
  // 不可变链表实现
  type List[T] = 
    | Nil
    | Cons(T, List[T])
  
  // 创建链表
  let list1 : List[Int] = Cons(1, Cons(2, Cons(3, Nil)))
  
  // 链表长度计算
  fn list_length[T](list : List[T]) : Int {
    match list {
      Nil => 0
      Cons(_, tail) => 1 + list_length(tail)
    }
  }
  
  // 链表映射
  fn list_map[T, U](list : List[T], func : (T) -> U) : List[U] {
    match list {
      Nil => Nil
      Cons(head, tail) => Cons(func(head), list_map(tail, func))
    }
  }
  
  // 链表过滤
  fn list_filter[T](list : List[T], predicate : (T) -> Bool) : List[T] {
    match list {
      Nil => Nil
      Cons(head, tail) => {
        if predicate(head) {
          Cons(head, list_filter(tail, predicate))
        } else {
          list_filter(tail, predicate)
        }
      }
    }
  }
  
  // 链表折叠（reduce）
  fn list_fold[T, U](list : List[T], initial : U, func : (U, T) -> U) : U {
    match list {
      Nil => initial
      Cons(head, tail) => list_fold(tail, func(initial, head), func)
    }
  }
  
  // 验证链表操作
  assert_eq(list_length(list1), 3)
  
  let doubled_list = list_map(list1, fn(x) { x * 2 })
  assert_eq(list_length(doubled_list), 3)
  
  let even_list = list_filter(list1, fn(x) { x % 2 == 0 })
  assert_eq(list_length(even_list), 1) // 只有2是偶数
  
  let sum = list_fold(list1, 0, fn(acc, x) { acc + x })
  assert_eq(sum, 6) // 1 + 2 + 3 = 6
  
  // 不可变二叉树实现
  type Tree[T] = 
    | Empty
    | Node(T, Tree[T], Tree[T])
  
  // 创建二叉树
  let tree : Tree[Int] = Node(5, 
    Node(3, Node(1, Empty, Empty), Node(4, Empty, Empty)),
    Node(7, Node(6, Empty, Empty), Node(9, Empty, Empty))
  )
  
  // 树的深度计算
  fn tree_depth[T](tree : Tree[T]) : Int {
    match tree {
      Empty => 0
      Node(_, left, right) => {
        let left_depth = tree_depth(left)
        let right_depth = tree_depth(right)
        1 + (if left_depth > right_depth { left_depth } else { right_depth })
      }
    }
  }
  
  // 树的节点数量
  fn tree_size[T](tree : Tree[T]) : Int {
    match tree {
      Empty => 0
      Node(_, left, right) => 1 + tree_size(left) + tree_size(right)
    }
  }
  
  // 验证树操作
  assert_eq(tree_depth(tree), 3)
  assert_eq(tree_size(tree), 7)
}

// Test 5: 模式匹配测试
test "模式匹配测试" {
  // 定义枚举类型
  type Color = 
    | Red
    | Green
    | Blue
    | RGB(Int, Int, Int)
  
  // 定义形状类型
  type Shape = 
    | Circle(Float)
    | Rectangle(Float, Float)
    | Triangle(Float, Float, Float)
  
  // 使用模式匹配处理颜色
  fn color_to_string(color : Color) : String {
    match color {
      Red => "Red"
      Green => "Green"
      Blue => "Blue"
      RGB(r, g, b) => "RGB(" + r.to_string() + ", " + g.to_string() + ", " + b.to_string() + ")"
    }
  }
  
  // 使用模式匹配计算面积
  fn calculate_area(shape : Shape) : Float {
    match shape {
      Circle(radius) => 3.14159 * radius * radius
      Rectangle(width, height) => width * height
      Triangle(a, b, c) => {
        // 使用海伦公式
        let s = (a + b + c) / 2.0
        (s * (s - a) * (s - b) * (s - c)).sqrt()
      }
    }
  }
  
  // 验证颜色模式匹配
  assert_eq(color_to_string(Red), "Red")
  assert_eq(color_to_string(Green), "Green")
  assert_eq(color_to_string(Blue), "Blue")
  assert_eq(color_to_string(RGB(255, 0, 128)), "RGB(255, 0, 128)")
  
  // 验证形状模式匹配
  let circle_area = calculate_area(Circle(5.0))
  assert_true(circle_area > 78.0 && circle_area < 79.0) // π * 5² ≈ 78.54
  
  let rectangle_area = calculate_area(Rectangle(4.0, 6.0))
  assert_eq(rectangle_area, 24.0)
  
  let triangle_area = calculate_area(Triangle(3.0, 4.0, 5.0))
  assert_true(triangle_area > 5.0 && triangle_area < 7.0) // 3-4-5三角形面积是6
  
  // 复杂模式匹配
  fn describe_number(num : Int) : String {
    match num {
      0 => "Zero"
      1 => "One"
      2 | 3 | 5 | 7 | 11 => "Prime number"
      n if n < 0 => "Negative number: " + n.to_string()
      n if n % 2 == 0 => "Even number: " + n.to_string()
      n => "Odd number: " + n.to_string()
    }
  }
  
  // 验证复杂模式匹配
  assert_eq(describe_number(0), "Zero")
  assert_eq(describe_number(1), "One")
  assert_eq(describe_number(7), "Prime number")
  assert_eq(describe_number(-5), "Negative number: -5")
  assert_eq(describe_number(8), "Even number: 8")
  assert_eq(describe_number(9), "Odd number: 9")
}

// Test 6: 尾递归优化测试
test "尾递归优化测试" {
  // 普通递归阶乘（可能导致栈溢出）
  fn factorial(n : Int) : Int {
    if n <= 1 {
      1
    } else {
      n * factorial(n - 1)
    }
  }
  
  // 尾递归阶乘（可优化为迭代）
  fn factorial_tail(n : Int, acc : Int) : Int {
    if n <= 1 {
      acc
    } else {
      factorial_tail(n - 1, n * acc)
    }
  }
  
  // 尾递归斐波那契数列
  fn fibonacci_tail(n : Int, a : Int, b : Int) : Int {
    if n == 0 {
      a
    } else {
      fibonacci_tail(n - 1, b, a + b)
    }
  }
  
  // 验证普通递归
  assert_eq(factorial(5), 120)
  assert_eq(factorial(6), 720)
  
  // 验证尾递归
  assert_eq(factorial_tail(5, 1), 120)
  assert_eq(factorial_tail(6, 1), 720)
  
  // 验证尾递归斐波那契
  assert_eq(fibonacci_tail(10, 0, 1), 55)
  assert_eq(fibonacci_tail(15, 0, 1), 610)
  
  // 尾递归列表处理
  fn sum_list_tail(list : Array[Int], acc : Int) : Int {
    if list.length() == 0 {
      acc
    } else {
      sum_list_tail(list.slice(1, list.length()), acc + list[0])
    }
  }
  
  // 验证尾递归列表处理
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  assert_eq(sum_list_tail(numbers, 0), 55)
}

// Test 7: 惰性求值测试
test "惰性求值测试" {
  // 模拟惰性求值结构
  type Lazy[T] = {
    evaluated : Bool,
    value : Option[T],
    thunk : () -> T
  }
  
  // 创建惰性值
  fn lazy[T](thunk : () -> T) : Lazy[T] {
    {
      "evaluated": false,
      "value": None,
      "thunk": thunk
    }
  }
  
  // 强制求值
  fn force[T](lazy_val : Lazy[T]) : (T, Lazy[T]) {
    if lazy_val.evaluated {
      match lazy_val.value {
        Some(v) => (v, lazy_val)
        None => {
          // 这种情况不应该发生
          let value = lazy_val.thunk()
          (value, {
            "evaluated": true,
            "value": Some(value),
            "thunk": lazy_val.thunk
          })
        }
      }
    } else {
      let value = lazy_val.thunk()
      (value, {
        "evaluated": true,
        "value": Some(value),
        "thunk": lazy_val.thunk
      })
    }
  }
  
  // 创建计算密集型操作的惰性值
  let mut computation_count = 0
  let expensive_computation = lazy(fn() {
    computation_count = computation_count + 1
    // 模拟复杂计算
    let mut result = 0
    for i in 0 ..< 1000 {
      result = result + i
    }
    result
  })
  
  // 验证惰性求值
  assert_false(expensive_computation.evaluated)
  assert_eq(computation_count, 0)
  
  // 第一次强制求值
  let (value1, updated_lazy1) = force(expensive_computation)
  expensive_computation = updated_lazy1
  assert_eq(value1, 499500) // 0 + 1 + ... + 999
  assert_true(expensive_computation.evaluated)
  assert_eq(computation_count, 1)
  
  // 第二次强制求值（不应该重新计算）
  let (value2, updated_lazy2) = force(expensive_computation)
  expensive_computation = updated_lazy2
  assert_eq(value2, 499500)
  assert_eq(computation_count, 1) // 计算次数没有增加
  
  // 惰性列表（模拟）
  let natural_numbers = {
    "current": 0,
    "generated": []
  }
  
  // 生成惰性列表的前n个元素
  fn take_n(lazy_list : { String : Any }, n : Int) : Array[Int] {
    let mut result = []
    let mut current = lazy_list["current"]
    
    for i in 0 ..< n {
      result = result + [current]
      current = current + 1
    }
    
    result
  }
  
  // 验证惰性列表
  let first_five = take_n(natural_numbers, 5)
  assert_eq(first_five, [0, 1, 2, 3, 4])
}

// Test 8: 函数组合和管道测试
test "函数组合和管道测试" {
  // 基本函数
  fn add(x : Int) : Int -> Int {
    fn(y : Int) : Int { x + y }
  }
  
  fn multiply(x : Int) : Int -> Int {
    fn(y : Int) : Int { x * y }
  }
  
  // 函数组合（从右到左）
  fn compose[A, B, C](f : (B) -> C, g : (A) -> B) : (A) -> C {
    fn(x : A) : C { f(g(x)) }
  }
  
  // 函数管道（从左到右）
  fn pipe[A, B, C](f : (A) -> B, g : (B) -> C) : (A) -> C {
    fn(x : A) : C { g(f(x)) }
  }
  
  // 验证函数组合
  let add_then_multiply = compose(multiply(2), add(5))
  assert_eq(add_then_multiply(10), 30) // (10 + 5) * 2 = 30
  
  // 验证函数管道
  let multiply_then_add = pipe(multiply(2), add(5))
  assert_eq(multiply_then_add(10), 25) // (10 * 2) + 5 = 25
  
  // 多重函数组合
  let add3 = add(3)
  let multiply2 = multiply(2)
  let add5 = add(5)
  
  let complex_composition = compose(add5, compose(multiply2, add3))
  assert_eq(complex_composition(10), 31) // ((10 + 3) * 2) + 5 = 31
  
  // 字符串处理管道
  fn to_uppercase(s : String) : String {
    let mut result = ""
    for char in s {
      result = result + char.to_uppercase()
    }
    result
  }
  
  fn add_exclamation(s : String) : String {
    s + "!"
  }
  
  fn wrap_brackets(s : String) : String {
    "[" + s + "]"
  }
  
  // 字符串处理管道
  let string_pipeline = pipe(to_uppercase, pipe(add_exclamation, wrap_brackets))
  assert_eq(string_pipeline("hello"), "[HELLO!]")
  
  // 条件函数组合
  fn when[A](predicate : (A) -> Bool, f : (A) -> A) : (A) -> A {
    fn(x : A) : A {
      if predicate(x) {
        f(x)
      } else {
        x
      }
    }
  }
  
  // 验证条件函数组合
  let double_when_positive = when(fn(x) { x > 0 }, multiply(2))
  assert_eq(double_when_positive(5), 10)
  assert_eq(double_when_positive(-5), -5) // 负数不变
}

// Test 9: 函数式错误处理测试
test "函数式错误处理测试" {
  // Result类型定义
  type Result[T, E] = 
    | Ok(T)
    | Err(E)
  
  // 可能失败的操作
  fn safe_divide(x : Int, y : Int) : Result[Int, String] {
    if y == 0 {
      Err("Division by zero")
    } else {
      Ok(x / y)
    }
  }
  
  fn safe_parse_int(s : String) : Result[Int, String] {
    match s.parse_int() {
      Some(i) => Ok(i)
      None => Err("Invalid integer: " + s)
    }
  }
  
  // 链式操作
  fn chain_operations() : Result[Int, String] {
    match safe_parse_int("10") {
      Ok(num) => {
        match safe_divide(num, 2) {
          Ok(result) => Ok(result * 3)
          Err(e) => Err(e)
        }
      }
      Err(e) => Err(e)
    }
  }
  
  // 验证链式操作
  match chain_operations() {
    Ok(result) => assert_eq(result, 15) // (10 / 2) * 3 = 15
    Err(_) => assert_true(false)
  }
  
  // 失败的链式操作
  fn failing_chain_operations() : Result[Int, String] {
    match safe_parse_int("10") {
      Ok(num) => {
        match safe_divide(num, 0) {
          Ok(result) => Ok(result * 3)
          Err(e) => Err(e)
        }
      }
      Err(e) => Err(e)
    }
  }
  
  // 验证失败的链式操作
  match failing_chain_operations() {
    Ok(_) => assert_true(false)
    Err(e) => assert_eq(e, "Division by zero")
  }
  
  // 高阶函数处理Result
  fn map_result[T, U, E](result : Result[T, E], func : (T) -> U) : Result[U, E] {
    match result {
      Ok(value) => Ok(func(value))
      Err(error) => Err(error)
    }
  }
  
  fn flat_map_result[T, U, E](result : Result[T, E], func : (T) -> Result[U, E]) : Result[U, E] {
    match result {
      Ok(value) => func(value)
      Err(error) => Err(error)
    }
  }
  
  // 验证高阶函数处理Result
  let parse_result = safe_parse_int("42")
  let mapped = map_result(parse_result, fn(x) { x * 2 })
  match mapped {
    Ok(value) => assert_eq(value, 84)
    Err(_) => assert_true(false)
  }
  
  let chained = flat_map_result(parse_result, fn(x) { safe_divide(x, 2) })
  match chained {
    Ok(value) => assert_eq(value, 21)
    Err(_) => assert_true(false)
  }
  
  // Either类型（另一种错误处理方式）
  type Either[L, R] = 
    | Left(L)
    | Right(R)
  
  fn safe_divide_either(x : Int, y : Int) : Either[String, Int] {
    if y == 0 {
      Left("Division by zero")
    } else {
      Right(x / y)
    }
  }
  
  // 验证Either类型
  match safe_divide_either(10, 2) {
    Right(result) => assert_eq(result, 5)
    Left(_) => assert_true(false)
  }
  
  match safe_divide_either(10, 0) {
    Right(_) => assert_true(false)
    Left(error) => assert_eq(error, "Division by zero")
  }
}

// Test 10: 函数式状态管理测试
test "函数式状态管理测试" {
  // 状态类型
  type State = {
    counter : Int,
    name : String,
    history : Array[String]
  }
  
  // 初始状态
  let initial_state : State = {
    "counter": 0,
    "name": "Azimuth",
    "history": []
  }
  
  // 状态转换函数
  type Transition[S] = (S) -> S
  
  // 应用状态转换
  fn apply_transition[S](state : S, transition : Transition[S]) : S {
    transition(state)
  }
  
  // 具体的状态转换
  fn increment_counter(state : State) : State {
    {
      "counter": state.counter + 1,
      "name": state.name,
      "history": state.history + ["incremented counter to " + (state.counter + 1).to_string()]
    }
  }
  
  fn update_name(state : State, new_name : String) : State {
    {
      "counter": state.counter,
      "name": new_name,
      "history": state.history + ["updated name to " + new_name]
    }
  }
  
  // 验证状态转换
  let state1 = apply_transition(initial_state, increment_counter)
  assert_eq(state1.counter, 1)
  assert_eq(state1.name, "Azimuth")
  assert_eq(state1.history.length(), 1)
  
  let state2 = apply_transition(state1, increment_counter)
  assert_eq(state2.counter, 2)
  assert_eq(state2.history.length(), 2)
  
  let state3 = apply_transition(state2, fn(s) { update_name(s, "MoonBit") })
  assert_eq(state3.counter, 2)
  assert_eq(state3.name, "MoonBit")
  assert_eq(state3.history.length(), 3)
  
  // 状态转换组合
  fn compose_transitions[S](t1 : Transition[S], t2 : Transition[S]) : Transition[S] {
    fn(state : S) : S {
      t2(t1(state))
    }
  }
  
  // 组合多个转换
  let increment_twice = compose_transitions(increment_counter, increment_counter)
  let state4 = apply_transition(state3, increment_twice)
  assert_eq(state4.counter, 4)
  assert_eq(state4.history.length(), 5)
  
  // 条件状态转换
  fn conditional_transition[S](predicate : (S) -> Bool, transition : Transition[S]) : Transition[S] {
    fn(state : S) : S {
      if predicate(state) {
        transition(state)
      } else {
        state
      }
    }
  }
  
  // 验证条件转换
  let increment_if_even = conditional_transition(fn(s) { s.counter % 2 == 0 }, increment_counter)
  
  let state5 = apply_transition(state4, increment_if_even) // counter是4，偶数，会递增
  assert_eq(state5.counter, 5)
  
  let state6 = apply_transition(state5, increment_if_even) // counter是5，奇数，不会递增
  assert_eq(state6.counter, 5)
  
  // 状态查询函数
  fn get_counter(state : State) : Int {
    state.counter
  }
  
  fn get_history(state : State) : Array[String] {
    state.history
  }
  
  // 验证状态查询
  assert_eq(get_counter(state6), 5)
  assert_eq(get_history(state6).length(), 6)
}