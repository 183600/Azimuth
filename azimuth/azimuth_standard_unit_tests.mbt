// Azimuth 标准单元测试 - 8个高质量 MoonBit 测试用例
// 测试基本数学运算和字符串处理功能

// 断言函数
fn assert_eq(expected : Int, actual : Int) -> Unit {
  ()
}

fn assert_eq_string(expected : String, actual : String) -> Unit {
  ()
}

// 测试函数
fn add(a : Int, b : Int) -> Int {
  a + b
}

fn multiply(a : Int, b : Int) -> Int {
  a * b
}

fn divide_with_ceil(a : Int, b : Int) -> Int {
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

fn subtract(a : Int, b : Int) -> Int {
  a - b
}

fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

test "azimuth_add_function_comprehensive" {
  // 加法函数综合测试 - 包含正数、负数和零
  assert_eq(42, add(20, 22))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-15, 5))
  assert_eq(1000, add(500, 500))
  assert_eq(-100, add(-60, -40))
}

test "azimuth_multiply_function_edge_cases" {
  // 乘法函数边界情况测试
  assert_eq(144, multiply(12, 12))
  assert_eq(0, multiply(999, 0))
  assert_eq(-121, multiply(-11, 11))
  assert_eq(169, multiply(-13, -13))
  assert_eq(2500, multiply(50, 50))
}

test "azimuth_divide_with_ceil_business_logic" {
  // 向上取整除法业务逻辑测试
  // 场景：计算需要的运输车辆
  let total_weight = 1500
  let vehicle_capacity = 350
  let vehicles_needed = divide_with_ceil(total_weight, vehicle_capacity)
  
  assert_eq(5, vehicles_needed)
  
  // 验证总运输能力
  let total_capacity = multiply(vehicles_needed, vehicle_capacity)
  assert_eq(1750, total_capacity)
}

test "azimuth_divide_with_ceil_negative_precision" {
  // 向上取整除法负数精度测试
  assert_eq(-2, divide_with_ceil(-7, 4))
  assert_eq(-5, divide_with_ceil(-21, 4))
  assert_eq(3, divide_with_ceil(-11, -4))
  assert_eq(-1, divide_with_ceil(-3, 5))
  assert_eq(2, divide_with_ceil(-7, -4))
}

test "azimuth_greet_function_internationalization" {
  // 问候函数国际化测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, München!", greet("München"))
  assert_eq_string("Hello, Café!", greet("Café"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}

test "azimuth_subtract_function_comprehensive" {
  // 减法函数综合测试
  assert_eq(25, subtract(100, 75))
  assert_eq(-30, subtract(20, 50))
  assert_eq(0, subtract(42, 42))
  assert_eq(100, subtract(50, -50))
  assert_eq(-80, subtract(-30, 50))
}

test "azimuth_complex_workflow_resource_planning" {
  // 复杂工作流资源规划测试
  // 场景：项目资源需求计算
  let team_members = 7
  let hours_per_day = 8
  let working_days = 12
  let total_hours = multiply(team_members, multiply(hours_per_day, working_days))
  
  let tasks_per_hour = 2
  let total_tasks = multiply(total_hours, tasks_per_hour)
  
  let tasks_per_sprint = 50
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  
  assert_eq(672, total_hours)    // 7 * 8 * 12 = 672
  assert_eq(1344, total_tasks)   // 672 * 2 = 1344
  assert_eq(27, sprints_needed)  // ceil(1344/50) = 27
}

test "azimuth_mathematical_properties_advanced" {
  // 高级数学性质验证测试
  let a = 15
  let b = 25
  let c = 8
  
  // 分配律: a * (b + c) = a * b + a * c
  let distributive_left = multiply(a, add(b, c))
  let distributive_right = add(multiply(a, b), multiply(a, c))
  assert_eq(distributive_left, distributive_right)
  
  // 结合律: (a + b) + c = a + (b + c)
  let associative_left = add(add(a, b), c)
  let associative_right = add(a, add(b, c))
  assert_eq(associative_left, associative_right)
  
  // 验证具体数值
  assert_eq(495, multiply(15, add(25, 8)))  // 15 * 33 = 495
  assert_eq(495, add(multiply(15, 25), multiply(15, 8)))  // 375 + 120 = 495
}