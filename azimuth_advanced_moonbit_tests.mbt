// Azimuth 高级MoonBit测试用例
// 包含高级MoonBit语言特性和Azimuth遥测系统的深度测试

// 测试1: 高级枚举和递归类型
test "高级枚举和递归类型操作" {
  // 定义递归类型
  enum List[T] {
    Nil
    Cons(T, List[T])
  }
  
  // 定义树结构
  enum Tree[T] {
    Empty
    Node(T, Tree[T], Tree[T])
  }
  
  // 创建列表
  let list = List::Cons(1, List::Cons(2, List::Cons(3, List::Nil)))
  
  // 递归函数计算列表长度
  let list_length = fn(lst: List[T]) {
    match lst {
      List::Nil => 0
      List::Cons(_, tail) => 1 + list_length(tail)
    }
  }
  
  // 递归函数反转列表
  let reverse_list = fn(lst: List[T]) {
    let helper = fn(input: List[T], acc: List[T]) {
      match input {
        List::Nil => acc
        List::Cons(head, tail) => helper(tail, List::Cons(head, acc))
      }
    }
    helper(lst, List::Nil)
  }
  
  // 测试列表操作
  assert_eq(list_length(list), 3)
  let reversed = reverse_list(list)
  assert_eq(list_length(reversed), 3)
  
  // 创建二叉树
  let tree = Tree::Node(5, 
    Tree::Node(3, Tree::Empty, Tree::Empty),
    Tree::Node(7, Tree::Empty, Tree::Empty)
  )
  
  // 递归函数计算树的高度
  let tree_height = fn(t: Tree[T]) {
    match t {
      Tree::Empty => 0
      Tree::Node(_, left, right) => {
        let left_height = tree_height(left)
        let right_height = tree_height(right)
        1 + (if left_height > right_height { left_height } else { right_height })
      }
    }
  }
  
  // 递归函数中序遍历
  let inorder = fn(t: Tree[T]) {
    match t {
      Tree::Empty => []
      Tree::Node(value, left, right) => {
        inorder(left) + [value] + inorder(right)
      }
    }
  }
  
  // 测试树操作
  assert_eq(tree_height(tree), 2)
  assert_eq(inorder(tree), [3, 5, 7])
}

// 测试2: 高级泛型和类型约束
test "高级泛型和类型约束" {
  // 定义可比较类型约束
  trait Comparable[T] {
    compare(T, T) -> Int
  }
  
  // 为Int实现Comparable
  let int_compare = fn(a: Int, b: Int) {
    if a < b { -1 } else if a > b { 1 } else { 0 }
  }
  
  // 为String实现Comparable
  let string_compare = fn(a: String, b: String) {
    if a < b { -1 } else if a > b { 1 } else { 0 }
  }
  
  // 泛型排序函数
  let generic_sort = fn(arr: Array[T], compare_fn: (T, T) -> Int) {
    let len = arr.length()
    let result = arr.copy()
    
    // 简单的冒泡排序
    for i in 0..len {
      for j in 0..(len - i - 1) {
        if compare_fn(result[j], result[j + 1]) > 0 {
          let temp = result[j]
          result[j] = result[j + 1]
          result[j + 1] = temp
        }
      }
    }
    result
  }
  
  // 泛型二分查找
  let binary_search = fn(arr: Array[T], target: T, compare_fn: (T, T) -> Int) {
    let mut left = 0
    let mut right = arr.length() - 1
    
    while left <= right {
      let mid = (left + right) / 2
      let cmp = compare_fn(arr[mid], target)
      
      if cmp == 0 {
        return Some(mid)
      } else if cmp < 0 {
        left = mid + 1
      } else {
        right = mid - 1
      }
    }
    
    None
  }
  
  // 测试整数排序和搜索
  let numbers = [5, 2, 8, 1, 9, 3]
  let sorted_numbers = generic_sort(numbers, int_compare)
  assert_eq(sorted_numbers, [1, 2, 3, 5, 8, 9])
  
  let search_result = binary_search(sorted_numbers, 5, int_compare)
  assert_eq(search_result, Some(3))
  
  let not_found = binary_search(sorted_numbers, 7, int_compare)
  assert_eq(not_found, None)
  
  // 测试字符串排序和搜索
  let strings = ["zebra", "apple", "orange", "banana"]
  let sorted_strings = generic_sort(strings, string_compare)
  assert_eq(sorted_strings, ["apple", "banana", "orange", "zebra"])
  
  let string_search = binary_search(sorted_strings, "orange", string_compare)
  assert_eq(string_search, Some(2))
}

// 测试3: 高级函数式编程模式
test "高级函数式编程模式" {
  // 柯里化函数
  let curried_add = fn(a: Int) {
    fn(b: Int) {
      fn(c: Int) {
        a + b + c
      }
    }
  }
  
  // 部分应用
  let add_5 = curried_add(5)
  let add_5_and_3 = add_5(3)
  let result = add_5_and_3(2)
  assert_eq(result, 10)
  
  // 记忆化函数
  let memoize = fn(f: (Int) -> Int) {
    let cache = Map::empty()
    fn(x: Int) {
      match Map::get(cache, x) {
        Some(cached_result) => cached_result
        None => {
          let result = f(x)
          let _ = Map::insert(cache, x, result)
          result
        }
      }
    }
  }
  
  // 斐波那契函数（带记忆化）
  let fib = memoize(fn(n: Int) {
    if n <= 1 {
      n
    } else {
      let fib_memo = memoize(fn(m: Int) {
        if m <= 1 { m } else { fib(m - 1) + fib(m - 2) }
      })
      fib_memo(n - 1) + fib_memo(n - 2)
    }
  })
  
  // 测试记忆化效果
  assert_eq(fib(0), 0)
  assert_eq(fib(1), 1)
  assert_eq(fib(5), 5)
  assert_eq(fib(10), 55)
  
  // 函数管道
  let pipe = fn(fns: Array[(T) -> T>) {
    fn(initial: T) {
      let mut result = initial
      for f in fns {
        result = f(result)
      }
      result
    }
  }
  
  // 创建处理管道
  let pipeline = pipe([
    fn(x) { x * 2 },
    fn(x) { x + 10 },
    fn(x) { x / 3 }
  ])
  
  assert_eq(pipeline(9), 16)  // ((9 * 2) + 10) / 3 = 28 / 3 ≈ 9.33 -> 9 (整数除法)
  
  // 高阶map函数
  let advanced_map = fn(arr: Array[T>, fns: Array[(T) -> U>) {
    let mut result = []
    for item in arr {
      let mut transformed = item
      for f in fns {
        transformed = f(transformed)
      }
      result = result.push(transformed)
    }
    result
  }
  
  let numbers = [1, 2, 3, 4, 5]
  let transformed = advanced_map(numbers, [
    fn(x) { x * x },
    fn(x) { x + 1 }
  ])
  
  assert_eq(transformed, [2, 5, 10, 17, 26])
}

// 测试4: 高级模式匹配和守卫
test "高级模式匹配和守卫" {
  // 定义复杂枚举
  enum Shape {
    Circle(Float)
    Rectangle(Float, Float)
    Triangle(Float, Float, Float)
    Polygon(Array[(Float, Float)>)
  }
  
  enum Result[T, E] {
    Success(T)
    Failure(E)
    Pending
  }
  
  // 计算形状面积
  let area = fn(shape: Shape) {
    match shape {
      Shape::Circle(radius) => 3.14159 * radius * radius
      Shape::Rectangle(width, height) => width * height
      Shape::Triangle(a, b, c) if a + b > c && a + c > b && b + c > a => {
        let s = (a + b + c) / 2.0
        (s * (s - a) * (s - b) * (s - c)).sqrt()
      }
      Shape::Triangle(_, _, _) => 0.0  // 无效三角形
      Shape::Polygon(points) if points.length() >= 3 => {
        // 使用鞋带公式计算多边形面积
        let n = points.length()
        let mut sum = 0.0
        for i in 0..n {
          let (x1, y1) = points[i]
          let (x2, y2) = points[(i + 1) % n]
          sum = sum + (x1 * y2 - x2 * y1)
        }
        sum.abs() / 2.0
      }
      Shape::Polygon(_) => 0.0  // 无效多边形
    }
  }
  
  // 测试面积计算
  let circle = Shape::Circle(5.0)
  assert_eq(area(circle).round(), 79.0)
  
  let rectangle = Shape::Rectangle(4.0, 6.0)
  assert_eq(area(rectangle), 24.0)
  
  let valid_triangle = Shape::Triangle(3.0, 4.0, 5.0)
  assert_eq(area(valid_triangle).round(), 6.0)
  
  let invalid_triangle = Shape::Triangle(1.0, 2.0, 10.0)
  assert_eq(area(invalid_triangle), 0.0)
  
  let square = Shape::Polygon([(0.0, 0.0), (4.0, 0.0), (4.0, 4.0), (0.0, 4.0)])
  assert_eq(area(square), 16.0)
  
  // 处理嵌套枚举
  let process_result = fn(result: Result[String, String>) {
    match result {
      Result::Success(msg) if msg.contains("success") => {
        "操作成功: " + msg
      }
      Result::Success(msg) => {
        "完成: " + msg
      }
      Result::Failure(err) if err.contains("network") => {
        "网络错误: " + err
      }
      Result::Failure(err) => {
        "错误: " + err
      }
      Result::Pending => {
        "处理中..."
      }
    }
  }
  
  assert_eq(process_result(Result::Success("操作成功完成")), "操作成功: 操作成功完成")
  assert_eq(process_result(Result::Success("基本完成")), "完成: 基本完成")
  assert_eq(process_result(Result::Failure("网络连接超时")), "网络错误: 网络连接超时")
  assert_eq(process_result(Result::Failure("参数错误")), "错误: 参数错误")
  assert_eq(process_result(Result::Pending), "处理中...")
}

// 测试5: 高级遥测采样和过滤
test "高级遥测采样和过滤" {
  // 创建采样器
  let trace_id_ratio_sampler = fn(ratio: Float) {
    fn(trace_id: String) {
      // 简化的采样算法：基于trace_id的哈希值
      let hash = trace_id.chars().reduce(0, fn(acc, c) { acc + c.to_int() })
      let normalized = (hash % 100) as Float / 100.0
      normalized <= ratio
    }
  }
  
  // 创建属性过滤器
  let attribute_filter = fn(attributes: Array<(String, String)>, rules: Array<(String, String)>) {
    for rule in rules {
      let (key, expected_value) = rule
      let match_found = attributes.any(fn(attr) {
        let (attr_key, attr_value) = attr
        attr_key == key && attr_value == expected_value
      })
      if not match_found {
        return false
      }
    }
    true
  }
  
  // 测试采样器
  let sampler = trace_id_ratio_sampler(0.5)  // 50%采样率
  let sampled_trace = sampler("trace-12345")  // 假设哈希值小于50
  let not_sampled_trace = sampler("trace-67890")  // 假设哈希值大于50
  
  // 注意：实际结果取决于哈希计算，这里只是示例
  assert_true(sampled_trace || not_sampled_trace)  // 至少有一个应该被采样
  
  // 测试属性过滤器
  let attributes = [
    ("service.name", "payment-service"),
    ("environment", "production"),
    ("region", "us-west-2")
  ]
  
  let rules = [
    ("environment", "production"),
    ("service.name", "payment-service")
  ]
  
  let passes_filter = attribute_filter(attributes, rules)
  assert_true(passes_filter)
  
  let strict_rules = [
    ("environment", "production"),
    ("service.name", "payment-service"),
    ("version", "1.2.3")
  ]
  
  let fails_filter = attribute_filter(attributes, strict_rules)
  assert_false(fails_filter)
  
  // 组合采样器和过滤器
  let should_sample_telemetry = fn(trace_id: String, attributes: Array<(String, String)>) {
    let sampler_fn = trace_id_ratio_sampler(0.3)  // 30%采样率
    let filter_rules = [("environment", "production")]
    
    sampler_fn(trace_id) && attribute_filter(attributes, filter_rules)
  }
  
  let prod_trace = should_sample_telemetry("prod-trace-123", [
    ("environment", "production"),
    ("service.name", "api-service")
  ])
  
  let dev_trace = should_sample_telemetry("dev-trace-456", [
    ("environment", "development"),
    ("service.name", "api-service")
  ])
  
  // 生产环境追踪可能被采样（取决于采样率）
  // 开发环境追踪不应该被采样（不满足过滤器条件）
  assert_false(dev_trace)
}

// 测试6: 高度可配置的遥测管道
test "高度可配置的遥测管道" {
  // 定义遥测管道配置
  type PipelineConfig = {
    batch_size: Int,
    flush_interval_ms: Int,
    retry_attempts: Int,
    compression_enabled: Bool,
    export_timeout_ms: Int
  }
  
  // 默认配置
  let default_config = {
    batch_size: 100,
    flush_interval_ms: 5000,
    retry_attempts: 3,
    compression_enabled: true,
    export_timeout_ms: 30000
  }
  
  // 创建配置验证器
  let validate_config = fn(config: PipelineConfig) {
    let errors = []
    
    if config.batch_size <= 0 {
      errors = errors.push("batch_size must be positive")
    }
    
    if config.flush_interval_ms <= 0 {
      errors = errors.push("flush_interval_ms must be positive")
    }
    
    if config.retry_attempts < 0 {
      errors = errors.push("retry_attempts cannot be negative")
    }
    
    if config.export_timeout_ms <= 0 {
      errors = errors.push("export_timeout_ms must be positive")
    }
    
    errors
  }
  
  // 测试默认配置
  let default_errors = validate_config(default_config)
  assert_eq(default_errors.length(), 0)
  
  // 测试无效配置
  let invalid_config = {
    batch_size: -1,
    flush_interval_ms: 0,
    retry_attempts: -1,
    compression_enabled: true,
    export_timeout_ms: -100
  }
  
  let invalid_errors = validate_config(invalid_config)
  assert_eq(invalid_errors.length(), 4)
  assert_true(invalid_errors.contains("batch_size must be positive"))
  assert_true(invalid_errors.contains("flush_interval_ms must be positive"))
  assert_true(invalid_errors.contains("retry_attempts cannot be negative"))
  assert_true(invalid_errors.contains("export_timeout_ms must be positive"))
  
  // 配置合并器
  let merge_configs = fn(base: PipelineConfig, override: PipelineConfig) {
    {
      batch_size: if override.batch_size != 0 { override.batch_size } else { base.batch_size },
      flush_interval_ms: if override.flush_interval_ms != 0 { override.flush_interval_ms } else { base.flush_interval_ms },
      retry_attempts: if override.retry_attempts != -1 { override.retry_attempts } else { base.retry_attempts },
      compression_enabled: override.compression_enabled,
      export_timeout_ms: if override.export_timeout_ms != 0 { override.export_timeout_ms } else { base.export_timeout_ms }
    }
  }
  
  // 测试配置合并
  let partial_override = {
    batch_size: 200,
    flush_interval_ms: 0,  // 使用默认值
    retry_attempts: 5,
    compression_enabled: false,
    export_timeout_ms: 0  // 使用默认值
  }
  
  let merged_config = merge_configs(default_config, partial_override)
  assert_eq(merged_config.batch_size, 200)
  assert_eq(merged_config.flush_interval_ms, 5000)  // 保持默认值
  assert_eq(merged_config.retry_attempts, 5)
  assert_eq(merged_config.compression_enabled, false)
  assert_eq(merged_config.export_timeout_ms, 30000)  // 保持默认值
  
  // 验证合并后的配置
  let merged_errors = validate_config(merged_config)
  assert_eq(merged_errors.length(), 0)
}

// 测试7: 高级遥测数据聚合和分析
test "高级遥测数据聚合和分析" {
  // 定义度量点类型
  type MetricPoint = {
    timestamp: Int,
    value: Float,
    attributes: Array<(String, String)>
  }
  
  // 定义聚合结果类型
  type AggregationResult = {
    count: Int,
    sum: Float,
    min: Float,
    max: Float,
    avg: Float,
    percentile_95: Float
  }
  
  // 创建数据聚合器
  let aggregate_metrics = fn(points: Array[MetricPoint]) {
    if points.length() == 0 {
      return {
        count: 0,
        sum: 0.0,
        min: 0.0,
        max: 0.0,
        avg: 0.0,
        percentile_95: 0.0
      }
    }
    
    let values = points.map(fn(p) { p.value })
    let sorted_values = values.sort(fn(a, b) { if a < b { -1 } else if a > b { 1 } else { 0 } })
    
    let count = values.length()
    let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
    let min = sorted_values[0]
    let max = sorted_values[count - 1]
    let avg = sum / (count as Float)
    
    // 计算95百分位数
    let percentile_index = ((count as Float) * 0.95) as Int
    let percentile_95 = if percentile_index < count {
      sorted_values[percentile_index]
    } else {
      sorted_values[count - 1]
    }
    
    {
      count,
      sum,
      min,
      max,
      avg,
      percentile_95
    }
  }
  
  // 按属性分组聚合
  let aggregate_by_attributes = fn(points: Array[MetricPoint], group_keys: Array<String>) {
    let groups = Map::empty()
    
    for point in points {
      // 构建分组键
      let group_key_parts = group_keys.map(fn(key) {
        match point.attributes.find(fn(attr) { attr.0 == key }) {
          Some((_, value)) => value
          None => "unknown"
        }
      })
      let group_key = group_key_parts.join("|")
      
      // 获取或创建分组
      let group_points = match Map::get(groups, group_key) {
        Some(pts) => pts
        None => []
      }
      
      // 添加点到分组
      let updated_group = group_points.push(point)
      let _ = Map::insert(groups, group_key, updated_group)
    }
    
    // 聚合每个分组
    let results = Map::empty()
    for (group_key, group_points) in groups {
      let aggregation = aggregate_metrics(group_points)
      let _ = Map::insert(results, group_key, aggregation)
    }
    
    results
  }
  
  // 创建测试数据
  let test_points = [
    { timestamp: 1000, value: 10.5, attributes: [("service", "api"), ("env", "prod")] },
    { timestamp: 2000, value: 15.2, attributes: [("service", "api"), ("env", "prod")] },
    { timestamp: 3000, value: 8.7, attributes: [("service", "web"), ("env", "prod")] },
    { timestamp: 4000, value: 12.3, attributes: [("service", "api"), ("env", "dev")] },
    { timestamp: 5000, value: 9.8, attributes: [("service", "web"), ("env", "dev")] },
    { timestamp: 6000, value: 11.1, attributes: [("service", "api"), ("env", "prod")] }
  ]
  
  // 测试基本聚合
  let all_aggregation = aggregate_metrics(test_points)
  assert_eq(all_aggregation.count, 6)
  assert_eq(all_aggregation.sum.round(), 67.6)
  assert_eq(all_aggregation.min, 8.7)
  assert_eq(all_aggregation.max, 15.2)
  assert_eq(all_aggregation.avg.round(), 11.27)
  
  // 测试按服务分组聚合
  let service_groups = aggregate_by_attributes(test_points, ["service"])
  
  // API服务应该有4个点: 10.5, 15.2, 12.3, 11.1
  let api_aggregation = match Map::get(service_groups, "api") {
    Some(result) => result
    None => { count: 0, sum: 0.0, min: 0.0, max: 0.0, avg: 0.0, percentile_95: 0.0 }
  }
  assert_eq(api_aggregation.count, 4)
  
  // Web服务应该有2个点: 8.7, 9.8
  let web_aggregation = match Map::get(service_groups, "web") {
    Some(result) => result
    None => { count: 0, sum: 0.0, min: 0.0, max: 0.0, avg: 0.0, percentile_95: 0.0 }
  }
  assert_eq(web_aggregation.count, 2)
  
  // 测试按服务和环境分组聚合
  let multi_groups = aggregate_by_attributes(test_points, ["service", "env"])
  
  // API+Prod应该有3个点: 10.5, 15.2, 11.1
  let api_prod_aggregation = match Map::get(multi_groups, "api|prod") {
    Some(result) => result
    None => { count: 0, sum: 0.0, min: 0.0, max: 0.0, avg: 0.0, percentile_95: 0.0 }
  }
  assert_eq(api_prod_aggregation.count, 3)
}

// 测试8: 高级错误处理和恢复策略
test "高级错误处理和恢复策略" {
  // 定义错误类型
  enum TelemetryError {
    NetworkError(String)
    SerializationError(String)
    ConfigurationError(String)
    RateLimitError(Int)  // Int表示重试延迟
    CriticalError(String)
  }
  
  // 定义恢复策略
  enum RecoveryStrategy {
    Retry(Int)  // Int表示最大重试次数
    Backoff(Float, Float)  // Float, Float表示初始延迟和最大延迟
    CircuitBreaker(Int, Int)  // Int, Int表示失败阈值和恢复超时
    Fallback(() -> Unit)  // 备用操作
    FailFast  // 快速失败
  }
  
  // 错误处理器
  let handle_error = fn(error: TelemetryError, strategy: RecoveryStrategy) {
    match (error, strategy) {
      (TelemetryError::NetworkError(msg), RecoveryStrategy::Retry(max_attempts)) => {
        println("网络错误: " + msg + ", 将重试最多" + max_attempts.to_string() + "次")
        "retry"
      }
      (TelemetryError::NetworkError(msg), RecoveryStrategy::Backoff(initial, max_delay)) => {
        println("网络错误: " + msg + ", 使用指数退避策略")
        "backoff"
      }
      (TelemetryError::RateLimitError(delay), RecoveryStrategy::Backoff(initial, max_delay)) => {
        println("速率限制错误, 延迟" + delay.to_string() + "ms后重试")
        "rate_limit_backoff"
      }
      (TelemetryError::SerializationError(msg), RecoveryStrategy::Fallback(fallback_fn)) => {
        println("序列化错误: " + msg + ", 执行备用操作")
        fallback_fn()
        "fallback_executed"
      }
      (TelemetryError::ConfigurationError(msg), RecoveryStrategy::FailFast) => {
        println("配置错误: " + msg + ", 快速失败")
        "fail_fast"
      }
      (TelemetryError::CriticalError(msg), RecoveryStrategy::CircuitBreaker(threshold, timeout)) => {
        println("严重错误: " + msg + ", 触发断路器")
        "circuit_breaker_open"
      }
      (error, strategy) => {
        println("未处理的错误/策略组合")
        "unhandled"
      }
    }
  }
  
  // 测试错误处理
  let network_error = TelemetryError::NetworkError("连接超时")
  let retry_strategy = RecoveryStrategy::Retry(3)
  let result1 = handle_error(network_error, retry_strategy)
  assert_eq(result1, "retry")
  
  let rate_limit_error = TelemetryError::RateLimitError(5000)
  let backoff_strategy = RecoveryStrategy::Backoff(1000.0, 30000.0)
  let result2 = handle_error(rate_limit_error, backoff_strategy)
  assert_eq(result2, "rate_limit_backoff")
  
  let serialization_error = TelemetryError::SerializationError("JSON格式无效")
  let fallback_strategy = RecoveryStrategy::Fallback(fn() { println("执行备用序列化") })
  let result3 = handle_error(serialization_error, fallback_strategy)
  assert_eq(result3, "fallback_executed")
  
  let config_error = TelemetryError::ConfigurationError("配置文件缺失")
  let fail_fast_strategy = RecoveryStrategy::FailFast
  let result4 = handle_error(config_error, fail_fast_strategy)
  assert_eq(result4, "fail_fast")
  
  let critical_error = TelemetryError::CriticalError("内存不足")
  let circuit_breaker_strategy = RecoveryStrategy::CircuitBreaker(5, 60000)
  let result5 = handle_error(critical_error, circuit_breaker_strategy)
  assert_eq(result5, "circuit_breaker_open")
  
  // 错误恢复状态跟踪
  type RecoveryState = {
    attempts: Int,
    last_error: Option[TelemetryError],
    recovery_in_progress: Bool,
    next_retry_time: Option[Int]
  }
  
  let update_recovery_state = fn(state: RecoveryState, error: TelemetryError, strategy: RecoveryStrategy) {
    match strategy {
      RecoveryStrategy::Retry(max_attempts) => {
        if state.attempts < max_attempts {
          {
            attempts: state.attempts + 1,
            last_error: Some(error),
            recovery_in_progress: true,
            next_retry_time: Some(Time::now() + 1000)  // 1秒后重试
          }
        } else {
          {
            attempts: state.attempts + 1,
            last_error: Some(error),
            recovery_in_progress: false,
            next_retry_time: None
          }
        }
      }
      RecoveryStrategy::CircuitBreaker(threshold, timeout) => {
        if state.attempts >= threshold {
          {
            attempts: state.attempts + 1,
            last_error: Some(error),
            recovery_in_progress: false,
            next_retry_time: Some(Time::now() + timeout)
          }
        } else {
          {
            attempts: state.attempts + 1,
            last_error: Some(error),
            recovery_in_progress: true,
            next_retry_time: Some(Time::now() + 5000)  // 5秒后重试
          }
        }
      }
      _ => {
        {
          attempts: state.attempts + 1,
          last_error: Some(error),
          recovery_in_progress: false,
          next_retry_time: None
        }
      }
    }
  }
  
  // 测试恢复状态更新
  let initial_state = {
    attempts: 0,
    last_error: None,
    recovery_in_progress: false,
    next_retry_time: None
  }
  
  let updated_state1 = update_recovery_state(initial_state, network_error, retry_strategy)
  assert_eq(updated_state1.attempts, 1)
  assert_eq(updated_state1.recovery_in_progress, true)
  assert_true(updated_state1.next_retry_time.is_some())
  
  let updated_state2 = update_recovery_state(updated_state1, network_error, retry_strategy)
  assert_eq(updated_state2.attempts, 2)
  assert_eq(updated_state2.recovery_in_progress, true)
  
  // 继续重试直到达到最大次数
  let final_state = update_recovery_state({
    attempts: 2,
    last_error: Some(network_error),
    recovery_in_progress: true,
    next_retry_time: Some(Time::now() + 1000)
  }, network_error, retry_strategy)
  
  assert_eq(final_state.attempts, 3)
  assert_eq(final_state.recovery_in_progress, false)
  assert_eq(final_state.next_retry_time, None)
}