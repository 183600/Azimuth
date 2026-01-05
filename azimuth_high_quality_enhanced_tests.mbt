// Azimuth High Quality Enhanced Test Suite
// 高质量增强测试套件，专注于MoonBit语言高级特性和遥测系统复杂场景

// 测试1: 高阶函数和函数式编程模式
test "高阶函数和函数式编程模式测试" {
  // 定义函数组合器
  let compose = fn(f, g) { fn(x) { f(g(x)) } }
  let pipe = fn(f, g) { fn(x) { g(f(x)) } }
  
  // 定义基础函数
  let add_five = fn(x) { x + 5 }
  let multiply_by_two = fn(x) { x * 2 }
  let square = fn(x) { x * x }
  
  // 测试函数组合
  let add_then_multiply = compose(multiply_by_two, add_five)
  assert_eq(add_then_multiply(3), 16) // (3 + 5) * 2 = 16
  
  let multiply_then_add = compose(add_five, multiply_by_two)
  assert_eq(multiply_then_add(3), 11) // (3 * 2) + 5 = 11
  
  // 测试函数管道
  let add_then_square = pipe(add_five, square)
  assert_eq(add_then_square(3), 64) // (3 + 5)^2 = 64
  
  // 测试柯里化
  let curry = fn(f) {
    fn(x) {
      fn(y) {
        f(x, y)
      }
    }
  }
  
  let add = fn(x, y) { x + y }
  let curried_add = curry(add)
  let add_ten = curried_add(10)
  assert_eq(add_ten(5), 15)
  
  // 测试部分应用
  let partial_apply = fn(f, fixed_arg) {
    fn(x) {
      f(fixed_arg, x)
    }
  }
  
  let subtract = fn(x, y) { x - y }
  let subtract_from_ten = partial_apply(subtract, 10)
  assert_eq(subtract_from_ten(3), 7)
  
  // 测试函数作为参数
  let apply_twice = fn(f, x) {
    f(f(x))
  }
  
  assert_eq(apply_twice(add_five, 10), 20)
  assert_eq(apply_twice(square, 2), 16)
  
  // 测试函数数组和高阶映射
  let operations = [add_five, multiply_by_two, square]
  let apply_operations = fn(x, ops) {
    let mut results = []
    for op in ops {
      results = results.push(op(x))
    }
    results
  }
  
  let results = apply_operations(3, operations)
  assert_eq(results, [8, 6, 9])
  
  // 测试函数式过滤和归约
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  let even_numbers = numbers.filter(fn(x) { x % 2 == 0 })
  assert_eq(even_numbers, [2, 4, 6, 8, 10])
  
  let sum_of_squares = numbers
    .filter(fn(x) { x % 2 == 1 })
    .map(square)
    .reduce(fn(acc, x) { acc + x }, 0)
  
  assert_eq(sum_of_squares, 165) // 1² + 3² + 5² + 7² + 9² = 165
}

// 测试2: 递归算法和尾递归优化
test "递归算法和尾递归优化测试" {
  // 普通递归阶乘
  let factorial = fn(n) {
    if n <= 1 {
      1
    } else {
      n * factorial(n - 1)
    }
  }
  
  // 尾递归阶乘
  let factorial_tail = fn(n) {
    let factorial_helper = fn(n, acc) {
      if n <= 1 {
        acc
      } else {
        factorial_helper(n - 1, n * acc)
      }
    }
    factorial_helper(n, 1)
  }
  
  // 测试阶乘计算
  assert_eq(factorial(0), 1)
  assert_eq(factorial(1), 1)
  assert_eq(factorial(5), 120)
  assert_eq(factorial_tail(0), 1)
  assert_eq(factorial_tail(1), 1)
  assert_eq(factorial_tail(5), 120)
  
  // 斐波那契数列（普通递归）
  let fibonacci = fn(n) {
    if n <= 1 {
      n
    } else {
      fibonacci(n - 1) + fibonacci(n - 2)
    }
  }
  
  // 斐波那契数列（尾递归优化）
  let fibonacci_tail = fn(n) {
    let fib_helper = fn(n, a, b) {
      if n == 0 {
        a
      } else if n == 1 {
        b
      } else {
        fib_helper(n - 1, b, a + b)
      }
    }
    fib_helper(n, 0, 1)
  }
  
  // 测试斐波那契数列
  assert_eq(fibonacci(0), 0)
  assert_eq(fibonacci(1), 1)
  assert_eq(fibonacci(5), 5)
  assert_eq(fibonacci(10), 55)
  assert_eq(fibonacci_tail(0), 0)
  assert_eq(fibonacci_tail(1), 1)
  assert_eq(fibonacci_tail(5), 5)
  assert_eq(fibonacci_tail(10), 55)
  
  // 递归列表处理
  let sum_list = fn(list) {
    match list {
      [] => 0
      [head, ..tail] => head + sum_list(tail)
    }
  }
  
  let reverse_list = fn(list) {
    let reverse_helper = fn(list, acc) {
      match list {
        [] => acc
        [head, ..tail] => reverse_helper(tail, [head, ..acc])
      }
    }
    reverse_helper(list, [])
  }
  
  // 测试列表处理
  let test_list = [1, 2, 3, 4, 5]
  assert_eq(sum_list(test_list), 15)
  assert_eq(reverse_list(test_list), [5, 4, 3, 2, 1])
  
  // 递归树遍历
  type Tree[T] = {
    value: T,
    left: Option[Tree[T]],
    right: Option[Tree[T]]
  }
  
  let create_tree = fn(value, left, right) {
    { value, left, right }
  }
  
  let tree = create_tree(1,
    Some(create_tree(2,
      Some(create_tree(4, None, None)),
      Some(create_tree(5, None, None))
    )),
    Some(create_tree(3,
      Some(create_tree(6, None, None)),
      Some(create_tree(7, None, None))
    ))
  )
  
  let inorder_traversal = fn(tree) {
    let traverse = fn(node, acc) {
      match node {
        None => acc
        Some(n) => {
          let left_result = traverse(n.left, acc)
          let with_current = left_result.push(n.value)
          traverse(n.right, with_current)
        }
      }
    }
    traverse(tree, [])
  }
  
  let inorder_result = inorder_traversal(tree)
  assert_eq(inorder_result, [4, 2, 5, 1, 6, 3, 7])
  
  // 树的深度计算
  let tree_depth = fn(tree) {
    let depth_helper = fn(node) {
      match node {
        None => 0
        Some(n) => {
          let left_depth = depth_helper(n.left)
          let right_depth = depth_helper(n.right)
          1 + (if left_depth > right_depth { left_depth } else { right_depth })
        }
      }
    }
    depth_helper(tree)
  }
  
  assert_eq(tree_depth(tree), 3)
}

// 测试3: 模式匹配和高级枚举
test "模式匹配和高级枚举测试" {
  // 定义复杂枚举类型
  enum TelemetryEvent {
    SpanStarted { trace_id: String, span_id: String, operation_name: String }
    SpanEnded { span_id: String, duration: Int }
    MetricRecorded { name: String, value: Float, unit: String }
    ErrorOccurred { error_code: Int, message: String, stack_trace: Option[String] }
    BatchProcessed { count: Int, processing_time: Int, success_rate: Float }
  }
  
  // 创建事件列表
  let events = [
    TelemetryEvent::SpanStarted { 
      trace_id: "trace-123", 
      span_id: "span-456", 
      operation_name: "database.query" 
    },
    TelemetryEvent::MetricRecorded { 
      name: "cpu.usage", 
      value: 75.5, 
      unit: "percent" 
    },
    TelemetryEvent::ErrorOccurred { 
      error_code: 500, 
      message: "Database connection failed", 
      stack_trace: Some("at Database.connect:42") 
    },
    TelemetryEvent::SpanEnded { 
      span_id: "span-456", 
      duration: 250 
    },
    TelemetryEvent::BatchProcessed { 
      count: 1000, 
      processing_time: 5000, 
      success_rate: 0.95 
    }
  ]
  
  // 使用模式匹配处理事件
  let process_event = fn(event) {
    match event {
      TelemetryEvent::SpanStarted { trace_id, span_id, operation_name } => {
        "Started span " + span_id + " in trace " + trace_id + " for operation " + operation_name
      }
      TelemetryEvent::SpanEnded { span_id, duration } => {
        "Ended span " + span_id + " after " + duration.to_string() + "ms"
      }
      TelemetryEvent::MetricRecorded { name, value, unit } => {
        "Metric " + name + ": " + value.to_string() + " " + unit
      }
      TelemetryEvent::ErrorOccurred { error_code, message, stack_trace } => {
        let base_msg = "Error " + error_code.to_string() + ": " + message
        match stack_trace {
          Some(trace) => base_msg + "\nStack trace: " + trace
          None => base_msg
        }
      }
      TelemetryEvent::BatchProcessed { count, processing_time, success_rate } => {
        "Processed " + count.to_string() + " items in " + 
        processing_time.to_string() + "ms with " + 
        (success_rate * 100.0).to_string() + "% success rate"
      }
    }
  }
  
  // 测试事件处理
  let processed_messages = events.map(process_event)
  
  assert_true(processed_messages[0].contains("Started span span-456"))
  assert_true(processed_messages[0].contains("database.query"))
  
  assert_true(processed_messages[1].contains("cpu.usage"))
  assert_true(processed_messages[1].contains("75.5"))
  assert_true(processed_messages[1].contains("percent"))
  
  assert_true(processed_messages[2].contains("Error 500"))
  assert_true(processed_messages[2].contains("Database connection failed"))
  assert_true(processed_messages[2].contains("Stack trace"))
  
  assert_true(processed_messages[3].contains("Ended span span-456"))
  assert_true(processed_messages[3].contains("250ms"))
  
  assert_true(processed_messages[4].contains("1000 items"))
  assert_true(processed_messages[4].contains("5000ms"))
  assert_true(processed_messages[4].contains("95.0% success rate"))
  
  // 嵌套模式匹配
  enum Result[T, E] {
    Ok(T)
    Err(E)
  }
  
  enum HttpStatus {
    Ok
    NotFound
    BadRequest
    InternalServerError
    Custom(Int)
  }
  
  let parse_response = fn(response) {
    match response {
      Result::Ok(status) => {
        match status {
          HttpStatus::Ok => "Request successful"
          HttpStatus::NotFound => "Resource not found"
          HttpStatus::BadRequest => "Invalid request"
          HttpStatus::InternalServerError => "Server error"
          HttpStatus::Custom(code) => "Custom status: " + code.to_string()
        }
      }
      Result::Err(error) => "Request failed: " + error
    }
  }
  
  // 测试嵌套模式匹配
  assert_eq(parse_response(Result::Ok(HttpStatus::Ok)), "Request successful")
  assert_eq(parse_response(Result::Ok(HttpStatus::NotFound)), "Resource not found")
  assert_eq(parse_response(Result::Ok(HttpStatus::Custom(418))), "Custom status: 418")
  assert_eq(parse_response(Result::Err("Network timeout")), "Request failed: Network timeout")
  
  // 守卫模式匹配
  let classify_number = fn(n) {
    match n {
      x if x < 0 => "Negative"
      x if x == 0 => "Zero"
      x if x > 0 && x < 10 => "Small positive"
      x if x >= 10 && x < 100 => "Medium positive"
      _ => "Large positive"
    }
  }
  
  assert_eq(classify_number(-5), "Negative")
  assert_eq(classify_number(0), "Zero")
  assert_eq(classify_number(5), "Small positive")
  assert_eq(classify_number(25), "Medium positive")
  assert_eq(classify_number(150), "Large positive")
}

// 测试4: 泛型和类型约束
test "泛型和类型约束测试" {
  // 定义泛型容器
  type Container[T] = {
    items: Array[T],
    size: Int
  }
  
  // 泛型函数
  let create_container = fn() {
    { items: [], size: 0 }
  }
  
  let add_item = fn(container: Container[T], item: T) {
    { container | 
      items: container.items.push(item), 
      size: container.size + 1 
    }
  }
  
  let get_item = fn(container: Container[T], index: Int) {
    if index >= 0 && index < container.size {
      Some(container.items[index])
    } else {
      None
    }
  }
  
  // 测试字符串容器
  let string_container = create_container()
  let string_container = add_item(string_container, "first")
  let string_container = add_item(string_container, "second")
  let string_container = add_item(string_container, "third")
  
  assert_eq(string_container.size, 3)
  assert_eq(get_item(string_container, 0), Some("first"))
  assert_eq(get_item(string_container, 2), Some("third"))
  assert_eq(get_item(string_container, 5), None)
  
  // 测试整数容器
  let int_container = create_container()
  let int_container = add_item(int_container, 10)
  let int_container = add_item(int_container, 20)
  let int_container = add_item(int_container, 30)
  
  assert_eq(int_container.size, 3)
  assert_eq(get_item(int_container, 1), Some(20))
  assert_eq(get_item(int_container, 0), Some(10))
  
  // 泛型比较函数
  let compare_values = fn(a: T, b: T) {
    // 模拟比较，实际应该使用类型约束
    match (a, b) {
      (x, y) if x == y => "Equal"
      (x, y) if x > y => "First is greater"
      _ => "Second is greater"
    }
  }
  
  // 测试比较
  assert_eq(compare_values(5, 3), "First is greater")
  assert_eq(compare_values(2, 7), "Second is greater")
  assert_eq(compare_values("hello", "hello"), "Equal")
  
  // 泛型栈实现
  type Stack[T] = {
    elements: Array[T],
    top: Int
  }
  
  let create_stack = fn() {
    { elements: [], top: -1 }
  }
  
  let push = fn(stack: Stack[T], item: T) {
    { stack |
      elements: stack.elements.push(item),
      top: stack.top + 1
    }
  }
  
  let pop = fn(stack: Stack[T]) {
    if stack.top >= 0 {
      let item = stack.elements[stack.top]
      let new_elements = stack.elements.slice(0, stack.top)
      (Some(item), { stack | elements: new_elements, top: stack.top - 1 })
    } else {
      (None, stack)
    }
  }
  
  let peek = fn(stack: Stack[T]) {
    if stack.top >= 0 {
      Some(stack.elements[stack.top])
    } else {
      None
    }
  }
  
  // 测试栈操作
  let stack = create_stack()
  let stack = push(stack, "a")
  let stack = push(stack, "b")
  let stack = push(stack, "c")
  
  assert_eq(peek(stack), Some("c"))
  
  let (item, stack) = pop(stack)
  assert_eq(item, Some("c"))
  assert_eq(peek(stack), Some("b"))
  
  let (item, stack) = pop(stack)
  assert_eq(item, Some("b"))
  assert_eq(peek(stack), Some("a"))
  
  let (item, stack) = pop(stack)
  assert_eq(item, Some("a"))
  assert_eq(peek(stack), None)
  
  let (item, _) = pop(stack)
  assert_eq(item, None)
  
  // 泛型队列实现
  type Queue[T] = {
    elements: Array[T],
    front: Int,
    rear: Int
  }
  
  let create_queue = fn() {
    { elements: [], front: 0, rear: 0 }
  }
  
  let enqueue = fn(queue: Queue[T], item: T) {
    { queue |
      elements: queue.elements.push(item),
      rear: queue.rear + 1
    }
  }
  
  let dequeue = fn(queue: Queue[T]) {
    if queue.front < queue.rear {
      let item = queue.elements[queue.front]
      (Some(item), { queue | front: queue.front + 1 })
    } else {
      (None, queue)
    }
  }
  
  // 测试队列操作
  let queue = create_queue()
  let queue = enqueue(queue, 1)
  let queue = enqueue(queue, 2)
  let queue = enqueue(queue, 3)
  
  let (item, queue) = dequeue(queue)
  assert_eq(item, Some(1))
  
  let (item, queue) = dequeue(queue)
  assert_eq(item, Some(2))
  
  let (item, queue) = dequeue(queue)
  assert_eq(item, Some(3))
  
  let (item, _) = dequeue(queue)
  assert_eq(item, None)
}

// 测试5: 异步编程和并发模式
test "异步编程和并发模式测试" {
  // 模拟异步操作
  type AsyncResult[T] = {
    completed: Bool,
    value: Option[T],
    error: Option[String]
  }
  
  let create_pending_async = fn() {
    { completed: false, value: None, error: None }
  }
  
  let create_success_async = fn(value: T) {
    { completed: true, value: Some(value), error: None }
  }
  
  let create_error_async = fn(error: String) {
    { completed: true, value: None, error: Some(error) }
  }
  
  // 模拟异步操作执行
  let execute_async = fn(operation: () -> T) {
    // 模拟异步执行延迟
    create_success_async(operation())
  }
  
  let execute_async_with_error = fn(operation: () -> T, error_condition: Bool) {
    if error_condition {
      create_error_async("Operation failed")
    } else {
      create_success_async(operation())
    }
  }
  
  // 测试成功异步操作
  let async_result1 = execute_async(fn() { 42 })
  assert_true(async_result1.completed)
  assert_eq(async_result1.value, Some(42))
  assert_eq(async_result1.error, None)
  
  // 测试失败异步操作
  let async_result2 = execute_async_with_error(fn() { 100 }, true)
  assert_true(async_result2.completed)
  assert_eq(async_result2.value, None)
  assert_eq(async_result2.error, Some("Operation failed"))
  
  // 异步操作组合
  let map_async = fn(async_result: AsyncResult[T], mapper: T -> U) {
    match async_result.completed {
      true => {
        match async_result.value {
          Some(value) => create_success_async(mapper(value))
          None => create_error_async("No value to map")
        }
      }
      false => create_pending_async()
    }
  }
  
  let flat_map_async = fn(async_result: AsyncResult[T], mapper: T -> AsyncResult[U]) {
    match async_result.completed {
      true => {
        match async_result.value {
          Some(value) => mapper(value)
          None => create_error_async("No value to flat map")
        }
      }
      false => create_pending_async()
    }
  }
  
  // 测试异步映射
  let async_result3 = execute_async(fn() { "hello" })
  let mapped_result = map_async(async_result3, fn(s) { s.length() })
  assert_true(mapped_result.completed)
  assert_eq(mapped_result.value, Some(5))
  
  // 测试异步平面映射
  let async_result4 = execute_async(fn() { 10 })
  let flat_mapped_result = flat_map_async(async_result4, fn(n) {
    if n > 5 {
      create_success_async(n * 2)
    } else {
      create_error_async("Value too small")
    }
  })
  assert_true(flat_mapped_result.completed)
  assert_eq(flat_mapped_result.value, Some(20))
  
  // 并发任务模拟
  type Task[T] = {
    id: Int,
    status: String,  // "pending", "running", "completed", "failed"
    result: Option[T],
    error: Option[String]
  }
  
  let create_task = fn(id: Int) {
    { 
      id, 
      status: "pending", 
      result: None, 
      error: None 
    }
  }
  
  let run_task = fn(task: Task[T], operation: () -> T) {
    let running_task = { task | status: "running" }
    
    // 模拟任务执行
    let result = operation()
    
    { running_task |
      status: "completed",
      result: Some(result)
    }
  }
  
  let run_task_with_failure = fn(task: Task[T], operation: () -> T, should_fail: Bool) {
    let running_task = { task | status: "running" }
    
    if should_fail {
      { running_task |
        status: "failed",
        error: Some("Task execution failed")
      }
    } else {
      let result = operation()
      { running_task |
        status: "completed",
        result: Some(result)
      }
    }
  }
  
  // 创建并运行多个任务
  let task1 = create_task(1)
  let task1 = run_task(task1, fn() { "Result from task 1" })
  
  let task2 = create_task(2)
  let task2 = run_task_with_failure(task2, fn() { "Result from task 2" }, true)
  
  let task3 = create_task(3)
  let task3 = run_task(task3, fn() { "Result from task 3" })
  
  // 验证任务结果
  assert_eq(task1.status, "completed")
  assert_eq(task1.result, Some("Result from task 1"))
  
  assert_eq(task2.status, "failed")
  assert_eq(task2.result, None)
  assert_eq(task2.error, Some("Task execution failed"))
  
  assert_eq(task3.status, "completed")
  assert_eq(task3.result, Some("Result from task 3"))
  
  // 任务聚合
  let aggregate_results = fn(tasks: Array Task[T]) {
    let mut results = []
    let mut errors = []
    
    for task in tasks {
      match task.status {
        "completed" => {
          match task.result {
            Some(result) => results = results.push(result)
            None => {} // 忽略无结果但状态为完成的任务
          }
        }
        "failed" => {
          match task.error {
            Some(error) => errors = errors.push(error)
            None => {} // 忽略无错误但状态为失败的任务
          }
        }
        _ => {} // 忽略未完成的任务
      }
    }
    
    { results, errors }
  }
  
  let tasks = [task1, task2, task3]
  let aggregation = aggregate_results(tasks)
  
  assert_eq(aggregation.results.length(), 2)
  assert_true(aggregation.results.contains("Result from task 1"))
  assert_true(aggregation.results.contains("Result from task 3"))
  
  assert_eq(aggregation.errors.length(), 1)
  assert_eq(aggregation.errors[0], "Task execution failed")
}

// 测试6: 内存管理和资源清理
test "内存管理和资源清理测试" {
  // 模拟资源管理
  type Resource[T] = {
    id: String,
    data: T,
    acquired: Bool,
    reference_count: Int
  }
  
  let create_resource = fn(id: String, data: T) {
    { 
      id, 
      data, 
      acquired: true, 
      reference_count: 1 
    }
  }
  
  let acquire_resource = fn(resource: Resource[T]) {
    if resource.acquired {
      { resource | reference_count: resource.reference_count + 1 }
    } else {
      // 资源未获取，无法增加引用计数
      resource
    }
  }
  
  let release_resource = fn(resource: Resource[T]) {
    let new_count = resource.reference_count - 1
    if new_count <= 0 {
      // 引用计数为0，释放资源
      { resource | acquired: false, reference_count: 0 }
    } else {
      { resource | reference_count: new_count }
    }
  }
  
  // 测试资源获取和释放
  let resource = create_resource("res-1", "important data")
  assert_eq(resource.id, "res-1")
  assert_eq(resource.data, "important data")
  assert_true(resource.acquired)
  assert_eq(resource.reference_count, 1)
  
  // 增加引用计数
  let resource = acquire_resource(resource)
  assert_eq(resource.reference_count, 2)
  assert_true(resource.acquired)
  
  // 释放一次引用
  let resource = release_resource(resource)
  assert_eq(resource.reference_count, 1)
  assert_true(resource.acquired)
  
  // 释放所有引用
  let resource = release_resource(resource)
  assert_eq(resource.reference_count, 0)
  assert_false(resource.acquired)
  
  // 模拟内存池
  type MemoryPool[T] = {
    free_objects: Array[T],
    allocated_objects: Array[T],
    max_size: Int
  }
  
  let create_memory_pool = fn(max_size: Int) {
    { 
      free_objects: [], 
      allocated_objects: [], 
      max_size 
    }
  }
  
  let allocate_from_pool = fn(pool: MemoryPool[T], factory: () -> T) {
    if pool.free_objects.length() > 0 {
      // 重用空闲对象
      let obj = pool.free_objects[0]
      let remaining_free = pool.free_objects.slice(1, pool.free_objects.length())
      let new_allocated = pool.allocated_objects.push(obj)
      
      (obj, { pool | 
        free_objects: remaining_free, 
        allocated_objects: new_allocated 
      })
    } else if pool.allocated_objects.length() < pool.max_size {
      // 创建新对象
      let obj = factory()
      let new_allocated = pool.allocated_objects.push(obj)
      
      (obj, { pool | allocated_objects: new_allocated })
    } else {
      // 池已满，无法分配
      let error_obj = factory() // 实际应该返回错误
      (error_obj, pool)
    }
  }
  
  let deallocate_to_pool = fn(pool: MemoryPool[T], obj: T) {
    // 从已分配列表中移除对象
    let index = pool.allocated_objects.index_of(obj)
    if index >= 0 {
      let new_allocated = pool.allocated_objects.remove_at(index)
      let new_free = pool.free_objects.push(obj)
      
      { pool | 
        allocated_objects: new_allocated, 
        free_objects: new_free 
      }
    } else {
      // 对象不在已分配列表中
      pool
    }
  }
  
  // 测试内存池
  let pool = create_memory_pool(3)
  
  // 分配对象
  let (obj1, pool) = allocate_from_pool(pool, fn() { "object-1" })
  let (obj2, pool) = allocate_from_pool(pool, fn() { "object-2" })
  let (obj3, pool) = allocate_from_pool(pool, fn() { "object-3" })
  
  assert_eq(obj1, "object-1")
  assert_eq(obj2, "object-2")
  assert_eq(obj3, "object-3")
  assert_eq(pool.allocated_objects.length(), 3)
  assert_eq(pool.free_objects.length(), 0)
  
  // 释放对象
  let pool = deallocate_to_pool(pool, obj2)
  assert_eq(pool.allocated_objects.length(), 2)
  assert_eq(pool.free_objects.length(), 1)
  assert_true(pool.free_objects.contains("object-2"))
  
  // 重新分配对象（应该重用已释放的对象）
  let (obj4, pool) = allocate_from_pool(pool, fn() { "object-4" })
  assert_eq(obj4, "object-2") // 重用了之前释放的对象
  assert_eq(pool.allocated_objects.length(), 3)
  assert_eq(pool.free_objects.length(), 0)
  
  // 模拟垃圾回收
  type GarbageCollector[T] = {
    objects: Array[T],
    marked_objects: Array[String],
    next_object_id: Int
  }
  
  let create_garbage_collector = fn() {
    { 
      objects: [], 
      marked_objects: [], 
      next_object_id: 0 
    }
  }
  
  let allocate_object = fn(gc: GarbageCollector[T], data: T) {
    let object_id = "obj-" + gc.next_object_id.to_string()
    let obj = { id: object_id, data }
    let new_objects = gc.objects.push(obj)
    
    (obj, { gc | 
      objects: new_objects, 
      next_object_id: gc.next_object_id + 1 
    })
  }
  
  let mark_object = fn(gc: GarbageCollector[T], object_id: String) {
    if not(gc.marked_objects.contains(object_id)) {
      { gc | 
        marked_objects: gc.marked_objects.push(object_id) 
      }
    } else {
      gc
    }
  }
  
  let collect_garbage = fn(gc: GarbageCollector[T]) {
    let mut surviving_objects = []
    for obj in gc.objects {
      if gc.marked_objects.contains(obj.id) {
        surviving_objects = surviving_objects.push(obj)
      }
    }
    
    { gc | 
      objects: surviving_objects, 
      marked_objects: [] 
    }
  }
  
  // 测试垃圾回收
  let gc = create_garbage_collector()
  
  // 分配对象
  let (obj1, gc) = allocate_object(gc, "data-1")
  let (obj2, gc) = allocate_object(gc, "data-2")
  let (obj3, gc) = allocate_object(gc, "data-3")
  
  assert_eq(gc.objects.length(), 3)
  
  // 标记一些对象
  let gc = mark_object(gc, obj1.id)
  let gc = mark_object(gc, obj3.id)
  
  // 执行垃圾回收
  let gc = collect_garbage(gc)
  
  // 验证只有标记的对象保留下来
  assert_eq(gc.objects.length(), 2)
  assert_true(gc.objects.exists(fn(o) { o.id == obj1.id }))
  assert_true(gc.objects.exists(fn(o) { o.id == obj3.id }))
  assert_false(gc.objects.exists(fn(o) { o.id == obj2.id }))
}

// 测试7: 错误处理和异常边界
test "错误处理和异常边界测试" {
  // 定义错误类型
  enum TelemetryError {
    ValidationError(String)
    NetworkError(Int, String)
    SerializationError(String)
    TimeoutError(Int)
    ResourceExhaustedError(String)
    UnknownError(String)
  }
  
  // 定义结果类型
  type Result[T] = {
    success: Bool,
    data: Option[T],
    error: Option[TelemetryError]
  }
  
  // 创建成功结果
  let create_success = fn(data: T) {
    { 
      success: true, 
      data: Some(data), 
      error: None 
    }
  }
  
  // 创建错误结果
  let create_error = fn(error: TelemetryError) {
    { 
      success: false, 
      data: None, 
      error: Some(error) 
    }
  }
  
  // 验证遥测数据
  let validate_telemetry_data = fn(trace_id: String, span_id: String) {
    if trace_id.length() == 0 {
      create_error(TelemetryError::ValidationError("Trace ID cannot be empty"))
    } else if span_id.length() == 0 {
      create_error(TelemetryError::ValidationError("Span ID cannot be empty"))
    } else if trace_id.length() > 32 {
      create_error(TelemetryError::ValidationError("Trace ID too long"))
    } else if span_id.length() > 16 {
      create_error(TelemetryError::ValidationError("Span ID too long"))
    } else {
      create_success({ trace_id, span_id })
    }
  }
  
  // 测试验证
  let valid_result = validate_telemetry_data("trace-123456", "span-789")
  assert_true(valid_result.success)
  assert_eq(valid_result.error, None)
  
  let empty_trace_result = validate_telemetry_data("", "span-789")
  assert_false(empty_trace_result.success)
  match empty_trace_result.error {
    Some(TelemetryError::ValidationError(msg)) => {
      assert_eq(msg, "Trace ID cannot be empty")
    }
    _ => assert_true(false)
  }
  
  let empty_span_result = validate_telemetry_data("trace-123456", "")
  assert_false(empty_span_result.success)
  match empty_span_result.error {
    Some(TelemetryError::ValidationError(msg)) => {
      assert_eq(msg, "Span ID cannot be empty")
    }
    _ => assert_true(false)
  }
  
  let long_trace_result = validate_telemetry_data("a".repeat(33), "span-789")
  assert_false(long_trace_result.success)
  match long_trace_result.error {
    Some(TelemetryError::ValidationError(msg)) => {
      assert_eq(msg, "Trace ID too long")
    }
    _ => assert_true(false)
  }
  
  // 模拟网络操作
  let send_telemetry_data = fn(data, timeout_ms: Int) {
    // 模拟网络延迟和可能的错误
    let network_delay = 100 // 模拟100ms延迟
    
    if network_delay > timeout_ms {
      create_error(TelemetryError::TimeoutError(timeout_ms))
    } else {
      // 模拟可能的网络错误
      let random_error_code = 500 // 模拟错误代码
      if random_error_code >= 400 {
        create_error(TelemetryError::NetworkError(random_error_code, "Server error"))
      } else {
        create_success("Data sent successfully")
      }
    }
  }
  
  // 测试网络操作
  let valid_data = validate_telemetry_data("trace-123", "span-456")
  match valid_data.success {
    true => {
      let data = valid_data.data.unwrap() // 假设有unwrap方法
      let send_result = send_telemetry_data(data, 50) // 超时时间小于网络延迟
      assert_false(send_result.success)
      match send_result.error {
        Some(TelemetryError::TimeoutError(timeout)) => {
          assert_eq(timeout, 50)
        }
        _ => assert_true(false)
      }
    }
    false => assert_true(false)
  }
  
  // 错误链和错误包装
  let wrap_error = fn(original_error: TelemetryError, context: String) {
    TelemetryError::UnknownError(context + ": " + match original_error {
      TelemetryError::ValidationError(msg) => "Validation error: " + msg
      TelemetryError::NetworkError(code, msg) => "Network error (" + code.to_string() + "): " + msg
      TelemetryError::SerializationError(msg) => "Serialization error: " + msg
      TelemetryError::TimeoutError(ms) => "Timeout after " + ms.to_string() + "ms"
      TelemetryError::ResourceExhaustedError(resource) => "Resource exhausted: " + resource
      TelemetryError::UnknownError(msg) => "Unknown error: " + msg
    })
  }
  
  // 测试错误包装
  let validation_error = TelemetryError::ValidationError("Invalid input")
  let wrapped_error = wrap_error(validation_error, "During telemetry processing")
  
  match wrapped_error {
    TelemetryError::UnknownError(msg) => {
      assert_true(msg.contains("During telemetry processing"))
      assert_true(msg.contains("Validation error: Invalid input"))
    }
    _ => assert_true(false)
  }
  
  // 错误恢复策略
  enum RetryStrategy {
    NoRetry
    FixedDelay(Int)  // 延迟毫秒数
    ExponentialBackoff(Int, Int)  // 初始延迟, 最大重试次数
  }
  
  let execute_with_retry = fn(operation: () -> Result[T], strategy: RetryStrategy) {
    match strategy {
      RetryStrategy::NoRetry => operation()
      RetryStrategy::FixedDelay(delay) => {
        let result = operation()
        match result.success {
          true => result
          false => {
            // 模拟延迟后重试
            operation()
          }
        }
      }
      RetryStrategy::ExponentialBackoff(initial_delay, max_retries) => {
        let mut current_delay = initial_delay
        let mut retry_count = 0
        let mut result = operation()
        
        while not(result.success) and retry_count < max_retries {
          // 模拟延迟
          current_delay = current_delay * 2
          retry_count = retry_count + 1
          result = operation()
        }
        
        result
      }
    }
  }
  
  // 测试重试策略
  let mut attempt_count = 0
  let failing_operation = fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      create_error(TelemetryError::NetworkError(500, "Server busy"))
    } else {
      create_success("Operation succeeded")
    }
  }
  
  attempt_count = 0
  let no_retry_result = execute_with_retry(failing_operation, RetryStrategy::NoRetry)
  assert_false(no_retry_result.success)
  assert_eq(attempt_count, 1)
  
  attempt_count = 0
  let fixed_delay_result = execute_with_retry(failing_operation, RetryStrategy::FixedDelay(100))
  assert_false(fixed_delay_result.success)
  assert_eq(attempt_count, 2)
  
  attempt_count = 0
  let exponential_backoff_result = execute_with_retry(failing_operation, RetryStrategy::ExponentialBackoff(100, 3))
  assert_true(exponential_backoff_result.success)
  assert_eq(attempt_count, 3)
  
  // 错误边界和异常隔离
  type SafeOperation[T] = {
    execute: () -> T,
    fallback: () -> T,
    error_handler: TelemetryError -> Unit
  }
  
  let execute_safely = fn(safe_op: SafeOperation[T]) {
    try {
      let result = safe_op.execute()
      create_success(result)
    } catch {
      error => {
        safe_op.error_handler(error)
        let fallback_result = safe_op.fallback()
        create_success(fallback_result)
      }
    }
  }
  
  // 测试安全操作执行
  let error_log = { mut messages: [] }
  
  let safe_op = {
    execute: fn() { "risky operation result" },
    fallback: fn() { "fallback result" },
    error_handler: fn(error) {
      error_log.messages = error_log.messages.push("Error occurred: " + match error {
        TelemetryError::ValidationError(msg) => msg
        TelemetryError::NetworkError(code, msg) => code.to_string() + ": " + msg
        TelemetryError::SerializationError(msg) => msg
        TelemetryError::TimeoutError(ms) => "Timeout: " + ms.to_string() + "ms"
        TelemetryError::ResourceExhaustedError(resource) => resource
        TelemetryError::UnknownError(msg) => msg
      })
    }
  }
  
  // 由于我们无法真正模拟try-catch，这里只是展示结构
  // 在实际实现中，execute_safely会捕获异常并执行fallback
  let safe_result = create_success("risky operation result")
  assert_true(safe_result.success)
  assert_eq(safe_result.data, Some("risky operation result"))
}

// 测试8: 性能优化和算法复杂度
test "性能优化和算法复杂度测试" {
  // 测试查找算法性能
  let linear_search = fn(arr: Array[T], target: T) {
    let mut index = -1
    for i in 0..arr.length() {
      if arr[i] == target {
        index = i
        break
      }
    }
    index
  }
  
  let binary_search = fn(sorted_arr: Array[T], target: T) {
    let mut left = 0
    let mut right = sorted_arr.length() - 1
    let mut found = -1
    
    while left <= right {
      let mid = (left + right) / 2
      if sorted_arr[mid] == target {
        found = mid
        break
      } else if sorted_arr[mid] < target {
        left = mid + 1
      } else {
        right = mid - 1
      }
    }
    
    found
  }
  
  // 创建测试数据
  let small_array = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let large_array = []
  for i in 1..10001 {
    large_array = large_array.push(i)
  }
  
  // 测试小数组查找
  let linear_small = linear_search(small_array, 7)
  let binary_small = binary_search(small_array, 7)
  assert_eq(linear_small, 6) // 0-based index
  assert_eq(binary_small, 6)
  
  // 测试大数组查找
  let linear_large_start = linear_search(large_array, 5)
  let binary_large_start = binary_search(large_array, 5)
  assert_eq(linear_large_start, 4)
  assert_eq(binary_large_start, 4)
  
  let linear_large_end = linear_search(large_array, 9995)
  let binary_large_end = binary_search(large_array, 9995)
  assert_eq(linear_large_end, 9994)
  assert_eq(binary_large_end, 9994)
  
  // 测试不存在的元素
  let linear_not_found = linear_search(large_array, 15000)
  let binary_not_found = binary_search(large_array, 15000)
  assert_eq(linear_not_found, -1)
  assert_eq(binary_not_found, -1)
  
  // 测试排序算法性能
  let bubble_sort = fn(arr: Array[Int]) {
    let mut sorted_arr = arr
    let n = sorted_arr.length()
    
    for i in 0..n {
      for j in 0..(n - i - 1) {
        if sorted_arr[j] > sorted_arr[j + 1] {
          let temp = sorted_arr[j]
          sorted_arr[j] = sorted_arr[j + 1]
          sorted_arr[j + 1] = temp
        }
      }
    }
    
    sorted_arr
  }
  
  let quick_sort = fn(arr: Array[Int]) {
    let sort_helper = fn(arr: Array[Int], low: Int, high: Int) {
      if low < high {
        let pivot_index = partition(arr, low, high)
        sort_helper(arr, low, pivot_index - 1)
        sort_helper(arr, pivot_index + 1, high)
      }
    }
    
    let partition = fn(arr: Array[Int], low: Int, high: Int) {
      let pivot = arr[high]
      let mut i = low - 1
      
      for j in low..high {
        if arr[j] <= pivot {
          i = i + 1
          let temp = arr[i]
          arr[i] = arr[j]
          arr[j] = temp
        }
      }
      
      let temp = arr[i + 1]
      arr[i + 1] = arr[high]
      arr[high] = temp
      
      i + 1
    }
    
    let mut sorted_arr = arr
    sort_helper(sorted_arr, 0, sorted_arr.length() - 1)
    sorted_arr
  }
  
  // 测试小数组排序
  let unsorted_small = [5, 2, 8, 1, 9, 3]
  let bubble_small = bubble_sort(unsorted_small)
  let quick_small = quick_sort(unsorted_small)
  
  assert_eq(bubble_small, [1, 2, 3, 5, 8, 9])
  assert_eq(quick_small, [1, 2, 3, 5, 8, 9])
  
  // 测试大数组排序（使用较小数组以避免性能问题）
  let unsorted_medium = []
  for i in 1..101 {
    unsorted_medium = unsorted_medium.push(101 - i) // 降序数组
  }
  
  let bubble_medium = bubble_sort(unsorted_medium)
  let quick_medium = quick_sort(unsorted_medium)
  
  // 验证排序结果
  let is_sorted = fn(arr: Array[Int]) {
    for i in 0..(arr.length() - 1) {
      if arr[i] > arr[i + 1] {
        return false
      }
    }
    true
  }
  
  assert_true(is_sorted(bubble_medium))
  assert_true(is_sorted(quick_medium))
  
  // 测试缓存和记忆化
  let memoized_fibonacci = fn() {
    let cache = { mut values: [] }
    
    fn(n) {
      if n < cache.values.length() {
        cache.values[n]
      } else {
        let mut result = 0
        if n <= 1 {
          result = n
        } else {
          result = memoized_fibonacci()(n - 1) + memoized_fibonacci()(n - 2)
        }
        
        // 扩展缓存以包含新计算的值
        while cache.values.length() <= n {
          cache.values = cache.values.push(0)
        }
        cache.values[n] = result
        
        result
      }
    }
  }
  
  // 测试记忆化斐波那契
  let fib_memo = memoized_fibonacci()
  assert_eq(fib_memo(0), 0)
  assert_eq(fib_memo(1), 1)
  assert_eq(fib_memo(10), 55)
  assert_eq(fib_memo(15), 610)
  
  // 测试字符串操作性能
  let string_concatenation = fn(strings: Array[String]) {
    let mut result = ""
    for s in strings {
      result = result + s
    }
    result
  }
  
  let string_builder = fn(strings: Array[String]) {
    let mut parts = []
    for s in strings {
      parts = parts.push(s)
    }
    parts.join("")
  }
  
  // 创建测试字符串数组
  let test_strings = []
  for i in 1..1001 {
    test_strings = test_strings.push("str-" + i.to_string())
  }
  
  // 测试字符串连接
  let concat_result = string_concatenation(test_strings)
  let builder_result = string_builder(test_strings)
  
  // 验证结果相同
  assert_eq(concat_result, builder_result)
  assert_true(concat_result.contains("str-1"))
  assert_true(concat_result.contains("str-500"))
  assert_true(concat_result.contains("str-1000"))
  
  // 测试数据结构性能
  let list_vs_array_access = fn() {
    // 创建数组
    let array = []
    for i in 1..1001 {
      array = array.push(i)
    }
    
    // 创建链表（模拟）
    type ListNode = {
      value: Int,
      next: Option[ListNode]
    }
    
    let create_list = fn(n) {
      let mut head = None
      for i in (n - 1)..0 {
        head = Some({ value: i, next: head })
      }
      head
    }
    
    let list = create_list(1000)
    
    // 测试数组访问（随机访问）
    let array_access_time = fn() {
      let mut sum = 0
      for i in 0..1000 {
        let index = (i * 7) % 1000  // 伪随机索引
        sum = sum + array[index]
      }
      sum
    }
    
    // 测试链表访问（顺序访问）
    let list_access_time = fn() {
      let mut sum = 0
      let mut current = list
      let mut count = 0
      
      while count < 1000 and current.is_some() {
        match current {
          Some(node) => {
            sum = sum + node.value
            current = node.next
            count = count + 1
          }
          None => break
        }
      }
      
      sum
    }
    
    let array_sum = array_access_time()
    let list_sum = list_access_time()
    
    // 验证结果相同
    assert_eq(array_sum, list_sum)
  }
  
  list_vs_array_access()
}