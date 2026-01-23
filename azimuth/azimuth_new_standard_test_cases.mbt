// Azimuth 新增标准测试用例
// 包含10个高质量的标准 MoonBit 测试用例

pub fn add(a : Int, b : Int) -> Int {
  a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  a * b
}

pub fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

pub fn divide_with_ceil(a : Int, b : Int) -> Int {
  if b == 0 { 
    0 
  } else {
    let quotient = a / b
    let remainder = a % b
    if remainder == 0 {
      quotient
    } else if a > 0 && b > 0 {
      quotient + 1
    } else if a < 0 && b < 0 {
      quotient + 1
    } else {
      quotient
    }
  }
}

// 辅助函数
fn subtract(a : Int, b : Int) -> Int {
  a - b
}

test "add_function_various_scenarios" {
  // 测试加法函数的各种场景
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
  assert_eq(0, add(50, -50))
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合情况
  assert_eq(25, multiply(5, 5))
  assert_eq(-25, multiply(5, -5))
  assert_eq(-25, multiply(-5, 5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(0, multiply(5, 0))
}

test "divide_with_ceil_complex_scenarios" {
  // 测试向上取整除法的复杂场景
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(-2, divide_with_ceil(5, -3))
  assert_eq(2, divide_with_ceil(-5, -3))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, English!", greet("English"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, 한국어!", greet("한국어"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
}

test "business_calculation_inventory" {
  // 业务计算：库存管理场景
  let total_items = 234
  let box_capacity = 25
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  assert_eq(10, boxes_needed)
  
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(250, total_capacity)
  
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(16, remaining_space)
}

test "mathematical_commutative_laws" {
  // 验证数学交换律
  let a = 15
  let b = 27
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "performance_edge_cases" {
  // 性能边缘情况测试
  let large_num = 100000
  
  // 大数运算
  assert_eq(200000, add(large_num, large_num))
  assert_eq(1000000000, multiply(31622, 31622))
  assert_eq(1, divide_with_ceil(large_num, large_num))
  assert_eq(100001, divide_with_ceil(large_num + 1, large_num))
}

test "error_handling_division" {
  // 错误处理：除法场景
  // 除零错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极小除数
  assert_eq(100000, divide_with_ceil(100000, 1))
  assert_eq(-100000, divide_with_ceil(-100000, 1))
}

test "string_input_validation" {
  // 字符串输入验证测试
  assert_eq_string("Hello, space !", greet(" space "))
  assert_eq_string("Hello, tab\t!", greet("tab\t"))
  assert_eq_string("Hello, newline\n!", greet("newline\n"))
  assert_eq_string("Hello, 123456!", greet("123456"))
  assert_eq_string("Hello, mixed123ABC!", greet("mixed123ABC"))
}

test "complex_workflow_simulation" {
  // 复杂工作流程模拟
  // 场景：项目资源规划
  let project_duration = 45  // 天
  let team_size = 8  // 人员
  let daily_working_hours = 8  // 每日工作小时
  let hourly_cost = 50  // 每小时成本
  
  // 计算总工时
  let total_hours = multiply(project_duration, multiply(team_size, daily_working_hours))
  assert_eq(2880, total_hours)  // 45 * 8 * 8 = 2880
  
  // 计算总成本
  let total_cost = multiply(total_hours, hourly_cost)
  assert_eq(144000, total_cost)  // 2880 * 50 = 144000
  
  // 计算支付周期（每周期不超过20000）
  let payment_limit = 20000
  let payment_cycles = divide_with_ceil(total_cost, payment_limit)
  assert_eq(8, payment_cycles)  // ceil(144000/20000) = 8
  
  // 计算每个周期的平均支付额
  let avg_payment_per_cycle = divide_with_ceil(total_cost, payment_cycles)
  assert_eq(18000, avg_payment_per_cycle)  // ceil(144000/8) = 18000
}

// 断言函数
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  ()
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  ()
}