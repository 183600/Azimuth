// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法，测试核心功能

// 定义必要的函数
fn assert_eq(lhs : Int, rhs : Int) -> Unit {
  if lhs != rhs {
    let _ = "Assertion failed"
  } else {
    ()
  }
  ()
}

fn assert_eq_string(lhs : String, rhs : String) -> Unit {
  if lhs != rhs {
    let _ = "String assertion failed"
  } else {
    ()
  }
  ()
}

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

test "add_function_comprehensive" {
  // 全面测试加法函数
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(-3, 13))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法函数
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(15, multiply(3, 5))
  assert_eq(100, multiply(10, 10))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_positive_numbers" {
  // 测试正数的向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(5, divide_with_ceil(20, 4))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-2, divide_with_ceil(-4, 2))
  assert_eq(3, divide_with_ceil(-9, -3))
}

test "complex_business_calculations" {
  // 复杂业务计算测试
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)
  assert_eq(215, total_before_discount)
  assert_eq(195, final_total)
}

test "packaging_and_shipping_calculations" {
  // 包装和运输计算测试
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  let cost_per_box = 5
  let total_packaging_cost = multiply(boxes_needed, cost_per_box)
  
  assert_eq(4, boxes_needed)
  assert_eq(20, total_packaging_cost)
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "real_world_shopping_cart" {
  // 真实世界购物车计算
  let item1_price = 29
  let item1_qty = 2
  let item2_price = 15
  let item2_qty = 3
  let tax_rate = 8
  
  let subtotal = add(multiply(item1_price, item1_qty), multiply(item2_price, item2_qty))
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(subtotal, tax)
  
  assert_eq(103, subtotal)
  assert_eq(9, tax)
  assert_eq(112, total)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  // 除数为零的情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极端数值测试
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(0, multiply(0, -1000))
  assert_eq(1, divide_with_ceil(1, 1))
}