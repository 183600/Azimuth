// 高质量 MoonBit 测试用例
// 针对 azimuth 库核心功能的标准测试

// 内置测试函数
fn assert_eq(lhs : Int, rhs : Int) -> Unit {
  if lhs != rhs {
    @builtin.abort("Assertion failed")
  } else {
    ()
  }
}

fn assert_eq_string(lhs : String, rhs : String) -> Unit {
  if lhs != rhs {
    @builtin.abort("String assertion failed")
  } else {
    ()
  }
}

// 测试用的函数实现
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

test "azimuth_core_addition_commutative" {
  // 测试加法交换律
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-3, -7))
}

test "azimuth_core_multiplication_identity" {
  // 测试乘法单位元和交换律
  assert_eq(25, multiply(25, 1))
  assert_eq(0, multiply(100, 0))
  assert_eq(36, multiply(6, 6))
  assert_eq(-18, multiply(-3, 6))
  assert_eq(multiply(12, 8), multiply(8, 12))
}

test "azimuth_core_divide_ceil_precision" {
  // 测试向上取整除法的精度
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "azimuth_core_greet_functionality" {
  // 测试问候函数的各种情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, User123!", greet("User123"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "azimuth_core_business_calculation" {
  // 测试实际业务计算场景
  let unit_price = 45
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(360, subtotal)
  assert_eq(375, total_before_discount)
  assert_eq(355, final_total)
}

test "azimuth_core_packaging_optimization" {
  // 测试包装优化计算
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 测试剩余空间
  let total_capacity = multiply(boxes_needed, items_per_box)
  let unused_space = add(total_capacity, -total_items)
  assert_eq(1, unused_space)
}

test "azimuth_core_financial_projection" {
  // 测试财务预测计算
  let monthly_revenue = 15000
  let growth_rate = 8
  let months = 6
  
  let total_revenue = multiply(monthly_revenue, months)
  let projected_growth = divide_with_ceil(multiply(total_revenue, growth_rate), 100)
  let projected_total = add(total_revenue, projected_growth)
  
  assert_eq(90000, total_revenue)
  assert_eq(7200, projected_growth)
  assert_eq(97200, projected_total)
}

test "azimuth_core_time_management" {
  // 测试时间管理计算
  let total_tasks = 67
  let tasks_per_day = 8
  let workdays_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let total_hours = multiply(workdays_needed, 8)
  
  assert_eq(9, workdays_needed)
  assert_eq(72, total_hours)
}

test "azimuth_core_complex_expression" {
  // 测试复杂表达式计算
  let x = 12
  let y = 8
  let z = 5
  
  // 计算：(x + y) * z - x / y
  let result = add(multiply(add(x, y), z), -divide_with_ceil(x, y))
  assert_eq(98, result)  // (12 + 8) * 5 - ceil(12/8) = 20 * 5 - 2 = 100 - 2 = 98
  
  // 计算嵌套表达式
  let nested = divide_with_ceil(multiply(add(x, multiply(y, z)), 3), 4)
  assert_eq(39, nested)  // ceil((12 + 8*5) * 3 / 4) = ceil(52 * 3 / 4) = ceil(156/4) = 39
}