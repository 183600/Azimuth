// Functional Composition Test Suite for Azimuth
// 函数组合测试套件 - 测试各种函数组合和函数式编程技术

// Test 1: 基本函数组合
test "basic function composition" {
  // 简单组合函数
  let compose = fn(f, g) {
    fn(x) { f(g(x)) }
  }
  
  // 基本函数
  let add_one = fn(x) { x + 1 }
  let double = fn(x) { x * 2 }
  let square = fn(x) { x * x }
  
  // 组合函数
  let add_one_then_double = compose(double, add_one)
  let double_then_square = compose(square, double)
  let add_one_then_double_then_square = compose(square, compose(double, add_one))
  
  // 测试组合
  assert_eq(add_one_then_double(5), 12)  // (5 + 1) * 2
  assert_eq(double_then_square(5), 100)  // (5 * 2)^2
  assert_eq(add_one_then_double_then_square(5), 144)  // ((5 + 1) * 2)^2
  
  // 多参数组合
  let compose2 = fn(f, g) {
    fn(x, y) { f(g(x, y)) }
  }
  
  let add = fn(x, y) { x + y }
  let multiply_by_3 = fn(x) { x * 3 }
  
  let add_then_multiply = compose2(multiply_by_3, add)
  assert_eq(add_then_multiply(2, 3), 15)  // (2 + 3) * 3
}

// Test 2: 管道操作
test "pipe operations" {
  // 管道操作符
  let pipe = fn(value, functions) {
    let mut result = value
    for f in functions {
      result = f(result)
    }
    result
  }
  
  // 管道操作
  let add_one = fn(x) { x + 1 }
  let double = fn(x) { x * 2 }
  let to_string = fn(x) { x.to_string() }
  
  let result = pipe(5, [add_one, double, to_string])
  assert_eq(result, "12")  // ((5 + 1) * 2).to_string()
  
  // 中间值管道
  let pipe_with_tee = fn(value, functions) {
    let mut result = value
    let mut history = [value]
    
    for f in functions {
      result = f(result)
      history.push(result)
    }
    
    (result, history)
  }
  
  let (final_result, history) = pipe_with_tee(3, [add_one, double, square])
  assert_eq(final_result, 16)  // ((3 + 1) * 2)^2
  assert_eq(history, [3, 4, 8, 16])
  
  // 条件管道
  let conditional_pipe = fn(value, predicates_and_functions) {
    let mut result = value
    
    for (predicate, func) in predicates_and_functions {
      if predicate(result) {
        result = func(result)
      }
    }
    
    result
  }
  
  let is_even = fn(x) { x % 2 == 0 }
  let is_positive = fn(x) { x > 0 }
  let double_if_even = fn(x) { if is_even(x) { x * 2 } else { x } }
  let add_one_if_positive = fn(x) { if is_positive(x) { x + 1 } else { x } }
  
  let conditional_result = conditional_pipe(
    4,
    [(is_even, double_if_even), (is_positive, add_one_if_positive)]
  )
  assert_eq(conditional_result, 9)  // 4 -> 8 -> 9
}

// Test 3: 柯里化和偏应用
test "currying_and_partial_application" {
  // 柯里化函数
  let curry = fn(f) {
    fn(a) {
      fn(b) {
        f(a, b)
      }
    }
  }
  
  // 普通函数
  let add = fn(a, b) { a + b }
  let multiply = fn(a, b) { a * b }
  
  // 柯里化版本
  let curried_add = curry(add)
  let curried_multiply = curry(multiply)
  
  // 使用柯里化函数
  let add_5 = curried_add(5)
  let multiply_by_3 = curried_multiply(3)
  
  assert_eq(add_5(10), 15)
  assert_eq(multiply_by_3(4), 12)
  
  // 偏应用
  let partial_apply = fn(f, fixed_args) {
    fn(args) {
      f(fixed_args + args)
    }
  }
  
  // 多参数函数
  let sum_three = fn(a, b, c) { a + b + c }
  
  // 偏应用版本
  let add_5_and_3 = partial_apply(sum_three, [5, 3])
  
  assert_eq(add_5_and_3([2]), 10)  // 5 + 3 + 2
  
  // 反转参数
  let flip = fn(f) {
    fn(a, b) { f(b, a) }
  }
  
  let subtract = fn(a, b) { a - b }
  let reverse_subtract = flip(subtract)
  
  assert_eq(subtract(10, 3), 7)
  assert_eq(reverse_subtract(10, 3), -3)  // 3 - 10
}

// Test 4: 高阶函数模式
test "higher_order_function_patterns" {
  // 映射-归约模式
  let map_reduce = fn(arr, map_fn, reduce_fn, initial) {
    let mut result = initial
    for item in arr {
      result = reduce_fn(result, map_fn(item))
    }
    result
  }
  
  let numbers = [1, 2, 3, 4, 5]
  
  // 计算平方和
  let sum_of_squares = map_reduce(
    numbers,
    fn(x) { x * x },
    fn(acc, x) { acc + x },
    0
  )
  assert_eq(sum_of_squares, 55)  // 1^2 + 2^2 + 3^2 + 4^2 + 5^2
  
  // 过滤-映射-归约组合
  let filter_map_reduce = fn(arr, filter_fn, map_fn, reduce_fn, initial) {
    let mut result = initial
    for item in arr {
      if filter_fn(item) {
        result = reduce_fn(result, map_fn(item))
      }
    }
    result
  }
  
  // 计算偶数的平方和
  let sum_of_even_squares = filter_map_reduce(
    numbers,
    fn(x) { x % 2 == 0 },
    fn(x) { x * x },
    fn(acc, x) { acc + x },
    0
  )
  assert_eq(sum_of_even_squares, 20)  // 2^2 + 4^2
  
  // 函数链
  let chain = fn(functions) {
    fn(initial_value) {
      let mut result = initial_value
      for f in functions {
        result = f(result)
      }
      result
    }
  }
  
  let operations = chain([
    fn(x) { x + 1 },
    fn(x) { x * 2 },
    fn(x) { x - 3 }
  ])
  
  assert_eq(operations(5), 9)  // ((5 + 1) * 2) - 3
}

// Test 5: 函数式数据结构
test "functional_data_structures" {
  // 不可变栈
  let empty_stack = { items: [] }
  
  let push = fn(stack, item) {
    { items: [item] + stack.items }
  }
  
  let pop = fn(stack) {
    match stack.items {
      [] => (None, stack)
      [head, ...tail] => (Some(head), { items: tail })
    }
  }
  
  // 使用不可变栈
  let stack1 = push(empty_stack, 1)
  let stack2 = push(stack1, 2)
  let stack3 = push(stack2, 3)
  
  let (top3, stack4) = pop(stack3)
  let (top2, stack5) = pop(stack4)
  let (top1, stack6) = pop(stack5)
  
  match (top3, top2, top1) {
    (Some(3), Some(2), Some(1)) => assert_true(true)
    _ => assert_true(false)
  }
  
  // 不可变链表
  let empty_list = None
  
  let cons = fn(head, tail) {
    Some({ head, tail })
  }
  
  let list_map = fn(list, f) {
    match list {
      None => None
      Some(node) => cons(f(node.head), list_map(node.tail, f))
    }
  }
  
  let list_filter = fn(list, predicate) {
    match list {
      None => None
      Some(node) => {
        let filtered_tail = list_filter(node.tail, predicate)
        if predicate(node.head) {
          cons(node.head, filtered_tail)
        } else {
          filtered_tail
        }
      }
    }
  }
  
  // 创建链表
  let list = cons(1, cons(2, cons(3, cons(4, cons(5, None)))))
  
  // 映射和过滤
  let doubled = list_map(list, fn(x) { x * 2 })
  let evens = list_filter(list, fn(x) { x % 2 == 0 })
  
  // 转换为数组以便测试
  let to_array = fn(list) {
    let mut result = []
    let mut current = list
    while let Some(node) = current {
      result.push(node.head)
      current = node.tail
    }
    result
  }
  
  assert_eq(to_array(doubled), [2, 4, 6, 8, 10])
  assert_eq(to_array(evens), [2, 4])
}

// Test 6: 函数式错误处理
test "functional_error_handling" {
  // Either类型
  type Either[L, R] {
    Left(L)
    Right(R)
  }
  
  // Either映射
  let either_map = fn(either, f) {
    match either {
      Left(l) => Left(l)
      Right(r) => Right(f(r))
    }
  }
  
  // Either链式操作
  let either_bind = fn(either, f) {
    match either {
      Left(l) => Left(l)
      Right(r) => f(r)
    }
  }
  
  // 安全除法
  let safe_divide = fn(a, b) {
    if b == 0 {
      Left("Division by zero")
    } else {
      Right(a / b)
    }
  }
  
  // 安全平方根
  let safe_sqrt = fn(x) {
    if x < 0 {
      Left("Negative number")
    } else {
      Right(sqrt(x))
    }
  }
  
  // 组合操作
  let divide_and_sqrt = fn(a, b) {
    either_bind(safe_divide(a, b), safe_sqrt)
  }
  
  // 测试
  assert_eq(divide_and_sqrt(16, 4), Right(2.0))
  assert_eq(divide_and_sqrt(16, 0), Left("Division by zero"))
  assert_eq(divide_and_sqrt(-16, 4), Left("Negative number"))
  
  // 验证函数
  let validate_positive = fn(x) {
    if x > 0 {
      Right(x)
    } else {
      Left("Not positive")
    }
  }
  
  let validate_even = fn(x) {
    if x % 2 == 0 {
      Right(x)
    } else {
      Left("Not even")
    }
  }
  
  // 组合验证
  let validate = fn(x) {
    either_bind(validate_positive(x), validate_even)
  }
  
  assert_eq(validate(4), Right(4))
  assert_eq(validate(3), Left("Not even"))
  assert_eq(validate(-2), Left("Not positive"))
}

// Test 7: 函数式迭代器
test "functional_iterators" {
  // 无限流
  let infinite_stream = fn(start, step) {
    let current = ref start
    
    fn() {
      let value = !current
      current := !current + step
      value
    }
  }
  
  // 限制流
  let take = fn(stream, n) {
    let mut results = []
    let count = ref 0
    
    while !count < n {
      results.push(stream())
      count := !count + 1
    }
    
    results
  }
  
  // 过滤流
  let filter_stream = fn(stream, predicate) {
    fn() {
      let mut value = stream()
      while !predicate(value) {
        value = stream()
      }
      value
    }
  }
  
  // 映射流
  let map_stream = fn(stream, transform) {
    fn() {
      let value = stream()
      transform(value)
    }
  }
  
  // 使用函数式迭代器
  let naturals = infinite_stream(1, 1)
  let evens = filter_stream(naturals, fn(x) { x % 2 == 0 })
  let doubled_evens = map_stream(evens, fn(x) { x * 2 })
  
  let first_5 = take(doubled_evens, 5)
  assert_eq(first_5, [4, 8, 12, 16, 20])
  
  // 斐波那契流
  let fibonacci_stream = fn() {
    let a = ref 0
    let b = ref 1
    
    fn() {
      let result = !a
      let next_a = !b
      let next_b = !a + !b
      a := next_a
      b := next_b
      result
    }
  }
  
  let fibs = fibonacci_stream()
  let first_10_fibs = take(fibs, 10)
  assert_eq(first_10_fibs, [0, 1, 1, 2, 3, 5, 8, 13, 21, 34])
}

// Test 8: 函数式状态管理
test "functional_state_management" {
  // 状态单子
  type State[S, A] {
    run: (S) -> (A, S)
  }
  
  // State构造器
  let pure_state = fn[S, A](value: A) {
    State[S, A] {
      run: fn(state) { (value, state) }
    }
  }
  
  // State绑定
  let bind_state = fn[S, A, B](state_a: State[S, A], f: (A) -> State[S, B]) {
    State[S, B] {
      run: fn(initial_state) {
        let (value, new_state) = state_a.run(initial_state)
        let next_state = f(value)
        next_state.run(new_state)
      }
    }
  }
  
  // 获取状态
  let get_state = fn[S]() {
    State[S, S] {
      run: fn(state) { (state, state) }
    }
  }
  
  // 设置状态
  let set_state = fn[S](new_state: S) {
    State[S, Unit] {
      run: fn(_) { ((), new_state) }
    }
  }
  
  // 修改状态
  let modify_state = fn[S](f: (S) -> S) {
    State[S, Unit] {
      run: fn(state) { ((), f(state)) }
    }
  }
  
  // 计数器状态
  let increment = fn() {
    modify_state(fn(count) { count + 1 })
  }
  
  let get_count = fn() {
    get_state()
  }
  
  // 组合状态操作
  let counter_program = bind_state(increment(), fn(_) {
    bind_state(increment(), fn(_) {
      bind_state(increment(), fn(_) {
        get_count()
      })
    })
  })
  
  let (final_count, _) = counter_program.run(0)
  assert_eq(final_count, 3)
  
  // 更复杂的状态操作
  let add_and_multiply = fn(a, b) {
    bind_state(pure_state(a), fn(x) {
      bind_state(pure_state(b), fn(y) {
        bind_state(modify_state(fn(sum) { sum + x + y }), fn(_) {
          bind_state(get_state(), fn(sum) {
            pure_state(sum * 2)
          })
        })
      })
    })
  }
  
  let (result, _) = add_and_multiply(3, 4).run(0)
  assert_eq(result, 14)  // ((0 + 3 + 4) * 2)
}

// Test 9: 函数式设计模式
test "functional_design_patterns" {
  // 函数式策略模式
  let strategy_sort = fn(arr, compare_strategy) {
    // 简单冒泡排序实现
    let mut result = arr.clone()
    let n = result.length()
    
    for i in 0..n {
      for j in 0..(n - i - 1) {
        if compare_strategy(result[j], result[j + 1]) > 0 {
          let temp = result[j]
          result[j] = result[j + 1]
          result[j + 1] = temp
        }
      }
    }
    
    result
  }
  
  let ascending = fn(a, b) { if a < b { -1 } else if a > b { 1 } else { 0 } }
  let descending = fn(a, b) { if a > b { -1 } else if a < b { 1 } else { 0 } }
  
  let numbers = [5, 2, 8, 1, 9, 3]
  
  let asc_sorted = strategy_sort(numbers, ascending)
  let desc_sorted = strategy_sort(numbers, descending)
  
  assert_eq(asc_sorted, [1, 2, 3, 5, 8, 9])
  assert_eq(desc_sorted, [9, 8, 5, 3, 2, 1])
  
  // 函数式访问者模式
  let visit_tree = fn(tree, visitor) {
    match tree {
      Leaf => visitor.leaf()
      Node(value, left, right) => {
        visitor.node(
          value,
          visit_tree(left, visitor),
          visit_tree(right, visitor)
        )
      }
    }
  }
  
  type Tree[T] {
    Leaf
    Node(T, Tree[T], Tree[T])
  }
  
  // 访问者
  let sum_visitor = {
    leaf: fn() { 0 },
    node: fn(value, left_sum, right_sum) { value + left_sum + right_sum }
  }
  
  let count_visitor = {
    leaf: fn() { 1 },
    node: fn(_, left_count, right_count) { 1 + left_count + right_count }
  }
  
  // 创建树
  let tree = Node(5, 
    Node(3, Leaf, Node(4, Leaf, Leaf)),
    Node(7, Leaf, Leaf)
  )
  
  let sum = visit_tree(tree, sum_visitor)
  let count = visit_tree(tree, count_visitor)
  
  assert_eq(sum, 19)  // 5 + 3 + 4 + 7
  assert_eq(count, 5)  // 5个节点
  
  // 函数式观察者模式
  let create_subject = fn() {
    let observers = ref []
    
    fn() {
      // 订阅
      fn(observer) {
        observers.push(observer)
      }
    },
    
    fn(value) {
      // 通知
      for observer in !observers {
        observer(value)
      }
    }
  }
  
  let (subscribe, notify) = create_subject()
  
  let mut received_values = []
  
  subscribe(fn(value) { received_values.push("Observer 1: " + value.to_string()) })
  subscribe(fn(value) { received_values.push("Observer 2: " + value.to_string()) })
  
  notify(42)
  notify("hello")
  
  assert_eq(received_values, [
    "Observer 1: 42",
    "Observer 2: 42",
    "Observer 1: hello",
    "Observer 2: hello"
  ])
}

// Test 10: 函数式优化技术
test "functional_optimization_techniques" {
  // 记忆化
  let memoize = fn(f) {
    let cache = ref {}
    
    fn(x) {
      if (!cache).contains(x) {
        let result = f(x)
        let new_cache = (!cache).set(x, result)
        cache := new_cache
      }
      (!cache).get(x).unwrap()
    }
  }
  
  // 昂贵的计算
  let expensive_fib = fn(n) {
    if n <= 1 {
      n
    } else {
      expensive_fib(n - 1) + expensive_fib(n - 2)
    }
  }
  
  let memoized_fib = memoize(expensive_fib)
  
  // 测试记忆化
  assert_eq(memoized_fib(10), 55)
  assert_eq(memoized_fib(15), 610)
  
  // 函数式惰性求值
  let lazy = fn(thunk) {
    let evaluated = ref false
    let value = ref None
    
    fn() {
      if !evaluated {
        value := Some(thunk())
        evaluated := true
      }
      (!value).unwrap()
    }
  }
  
  let lazy_value = lazy(fn() {
    // 模拟昂贵计算
    let mut sum = 0
    for i in 0..1000 {
      sum = sum + i
    }
    sum
  })
  
  // 第一次调用会计算
  let first_result = lazy_value()
  assert_eq(first_result, 499500)
  
  // 第二次调用返回缓存结果
  let second_result = lazy_value()
  assert_eq(second_result, 499500)
  
  // 函数式短路求值
  let all = fn(predicates) {
    for p in predicates {
      if !p() {
        return false
      }
    }
    true
  }
  
  let any = fn(predicates) {
    for p in predicates {
      if p() {
        return true
      }
    }
    false
  }
  
  let mut call_count = 0
  let expensive_check = fn() {
    call_count = call_count + 1
    true
  }
  
  // 测试短路求值
  let all_result = all([fn() { false }, expensive_check])
  assert_false(all_result)
  assert_eq(call_count, 0)  // expensive_check不应该被调用
  
  call_count = 0
  let any_result = any([fn() { true }, expensive_check])
  assert_true(any_result)
  assert_eq(call_count, 0)  // expensive_check不应该被调用
}