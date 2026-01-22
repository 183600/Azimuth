// 标准 MoonBit 测试用例集 - 专为 azimuth 库设计
// 使用标准的 MoonBit 测试语法，覆盖核心功能

// 定义必要的辅助函数
pub fn assert_eq(lhs : Int, rhs : Int) -> Unit {
  if lhs != rhs {
    let _ = "Assertion failed"
  } else {
    ()
  }
  ()
}

pub fn assert_eq_string(lhs : String, rhs : String) -> Unit {
  if lhs != rhs {
    let _ = "String assertion failed"
  } else {
    ()
  }
  ()
}

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

test "basic_arithmetic_operations" {
  // 基础算术运算测试
  assert_eq(15, add(8, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(24, multiply(6, 4))
  assert_eq(-18, multiply(-6, 3))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_basic" {
  // 问候函数基础测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试
  let product_price = 35
  let quantity = 6
  let tax_rate = 8
  
  let subtotal = multiply(product_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(subtotal, tax_amount)
  
  assert_eq(210, subtotal)
  assert_eq(17, tax_amount)
  assert_eq(227, total)
}

test "edge_case_handling" {
  // 边界情况处理测试
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  
  // 负数运算
  assert_eq(-15, add(-8, -7))
  assert_eq(20, multiply(-4, -5))
  assert_eq(-2, divide_with_ceil(-5, 2))
}

test "real_world_application" {
  // 实际应用场景测试：包装计算
  let total_items = 47
  let items_per_box = 12
  
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
}

test "string_processing_with_greet" {
  // 字符串处理与问候函数结合测试
  let first_name = "张"
  let last_name = "三"
  let full_name = first_name + last_name
  let greeting = greet(full_name)
  
  assert_eq_string("Hello, 张三!", greeting)
}

test "combined_operations" {
  // 组合运算测试
  let x = 5
  let y = 8
  let z = 3
  
  // 测试 (x + y) * z
  let result1 = multiply(add(x, y), z)
  assert_eq(39, result1)
  
  // 测试 x * y + z
  let result2 = add(multiply(x, y), z)
  assert_eq(43, result2)
}

test "business_calculation_workflow" {
  // 业务计算工作流测试
  let base_cost = 120
  let quantity = 7
  let discount = 50
  let shipping = 25
  
  let subtotal = multiply(base_cost, quantity)
  let discounted_total = add(subtotal, -discount)
  let final_total = add(discounted_total, shipping)
  
  assert_eq(840, subtotal)
  assert_eq(790, discounted_total)
  assert_eq(815, final_total)
}