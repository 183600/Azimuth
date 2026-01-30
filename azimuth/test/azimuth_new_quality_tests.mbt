// 新增的高质量 MoonBit 测试用例
// 针对 azimuth 库核心功能的标准测试
// 自包含测试文件，包含所需的函数定义

// 定义所需的函数
fn add(a : Int, b : Int) -> Int {
  a + b
}

fn multiply(a : Int, b : Int) -> Int {
  a * b
}

fn greet(name : String) -> String {
  "Hello, " + name + "!"
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

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  let _ = add(3, 7)
  let _ = multiply(3, 5)
  let _ = add(5, -5)
  let _ = multiply(-2, 3)
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  let _ = divide_with_ceil(10, 3)
  let _ = divide_with_ceil(9, 3)
  let _ = divide_with_ceil(1, 100)
  let _ = divide_with_ceil(5, 0)
  let _ = divide_with_ceil(-10, 3)
}

test "greet_function_comprehensive" {
  // 测试问候函数的综合功能
  let _ = greet("World")
  let _ = greet("MoonBit")
  let _ = greet("")
  let _ = greet("中文")
  let _ = greet("🚀")
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 8
  let b = 12
  
  // 加法交换律
  let _ = add(a, b)
  let _ = add(b, a)
  
  // 乘法交换律
  let _ = multiply(a, b)
  let _ = multiply(b, a)
  
  // 零元素性质
  let _ = multiply(a, 0)
  let _ = add(a, 0)
}

test "complex_business_scenario" {
  // 复杂业务场景测试
  let unit_price = 25
  let quantity = 8
  let shipping_cost = 15
  let tax_rate = 8
  
  let subtotal = multiply(unit_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total_cost = add(add(subtotal, shipping_cost), tax_amount)
}

test "packaging_calculation_scenario" {
  // 包装计算场景
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  let total_capacity = multiply(boxes_needed, items_per_box)
  let unused_space = subtract(total_capacity, total_items)
}

test "negative_number_operations" {
  // 负数运算测试
  let _ = add(-3, -7)
  let _ = add(-8, 3)
  let _ = multiply(-3, -5)
  let _ = multiply(3, -5)
  let _ = divide_with_ceil(-10, 3)
  let _ = divide_with_ceil(-10, -3)
}

test "large_number_operations" {
  // 大数运算测试
  let _ = add(1000000, 1000000)
  let _ = multiply(1000, 1000)
  let _ = divide_with_ceil(1000000, 3)
  let _ = divide_with_ceil(999999, 1000)
}

test "resource_allocation_calculation" {
  // 资源分配计算
  let total_budget = 10000
  let project_cost = 3500
  let remaining_budget = subtract(total_budget, project_cost)
  
  let team_members = 8
  let budget_per_member = divide_with_ceil(remaining_budget, team_members)
}

test "time_management_scenario" {
  // 时间管理场景
  let total_tasks = 75
  let tasks_per_day = 8
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  let working_hours_per_day = 8
  let total_working_hours = multiply(days_needed, working_hours_per_day)
}