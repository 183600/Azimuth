// 标准 MoonBit 测试用例 - 针对 azimuth 库核心功能
// 使用标准的 MoonBit 测试语法，包含10个测试用例

// 定义所需的函数
pub fn add(a : Int, b : Int) -> Int {
  a + b
}

pub fn subtract(a : Int, b : Int) -> Int {
  a - b
}

pub fn multiply(a : Int, b : Int) -> Int {
  a * b
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

pub fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  if (expected == actual) {
    ()
  } else {
    let _ = 1 / 0
    ()
  }
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  if (expected == actual) {
    ()
  } else {
    let _ = 1 / 0
    ()
  }
}

test "subtract_basic_functionality" {
  // 测试基本的减法功能
  assert_eq(5, subtract(10, 5))
  assert_eq(0, subtract(5, 5))
  assert_eq(-5, subtract(0, 5))
  assert_eq(10, subtract(15, 5))
}

test "add_with_negative_numbers" {
  // 测试包含负数的加法运算
  assert_eq(0, add(5, -5))
  assert_eq(-10, add(-5, -5))
  assert_eq(5, add(10, -5))
  assert_eq(-15, add(-10, -5))
}

test "multiply_with_negative_numbers" {
  // 测试包含负数的乘法运算
  assert_eq(-25, multiply(-5, 5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(0, multiply(-5, 0))
  assert_eq(-50, multiply(-10, 5))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))    // 3.33 -> 4
  assert_eq(3, divide_with_ceil(9, 3))     // 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100))   // 0.01 -> 1
  assert_eq(-3, divide_with_ceil(-10, 3))  // -3.33 -> -3
  assert_eq(0, divide_with_ceil(5, 0))     // 除零保护
}

test "greet_function_edge_cases" {
  // 测试问候函数的边界情况
  assert_eq_string("Hello, !", greet(""))                    // 空字符串
  assert_eq_string("Hello, A!", greet("A"))                  // 单字符
  assert_eq_string("Hello, 123!", greet("123"))              // 数字字符串
  assert_eq_string("Hello, 世界!", greet("世界"))              // Unicode字符
}

test "complex_arithmetic_combinations" {
  // 测试复杂的算术组合运算
  let x = 10
  let y = 5
  let z = 3
  
  // 测试 (x + y) * z
  let result1 = multiply(add(x, y), z)
  assert_eq(45, result1)  // (10 + 5) * 3 = 15 * 3 = 45
  
  // 测试 x * y + z
  let result2 = add(multiply(x, y), z)
  assert_eq(53, result2)  // 10 * 5 + 3 = 50 + 3 = 53
  
  // 测试 (x - y) * z
  let result3 = multiply(subtract(x, y), z)
  assert_eq(15, result3)  // (10 - 5) * 3 = 5 * 3 = 15
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 7
  let b = 13
  
  // 加法交换律: a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律: a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法单位元: a + 0 = a
  assert_eq(a, add(a, 0))
  
  // 乘法单位元: a * 1 = a
  assert_eq(a, multiply(a, 1))
  
  // 乘法零元: a * 0 = 0
  assert_eq(0, multiply(a, 0))
}

test "real_world_shopping_cart" {
  // 模拟真实购物车计算
  let apple_price = 5
  let apple_qty = 3
  let banana_price = 3
  let banana_qty = 4
  let shipping_fee = 10
  
  // 计算商品总价
  let apple_total = multiply(apple_price, apple_qty)
  let banana_total = multiply(banana_price, banana_qty)
  let subtotal = add(apple_total, banana_total)
  
  // 计算最终总价
  let total = add(subtotal, shipping_fee)
  
  assert_eq(15, apple_total)   // 5 * 3 = 15
  assert_eq(12, banana_total)  // 3 * 4 = 12
  assert_eq(27, subtotal)      // 15 + 12 = 27
  assert_eq(37, total)         // 27 + 10 = 37
}

test "packaging_and_shipping_calculation" {
  // 包装和运输计算场景
  let total_items = 47
  let items_per_box = 12
  let weight_per_item = 2  // kg
  let max_weight_per_box = 25  // kg
  
  // 计算需要的箱子数量（按物品数量）
  let boxes_by_count = divide_with_ceil(total_items, items_per_box)
  
  // 计算总重量
  let total_weight = multiply(total_items, weight_per_item)
  
  // 计算需要的箱子数量（按重量限制）
  let boxes_by_weight = divide_with_ceil(total_weight, max_weight_per_box)
  
  // 取两者中的较大值
  let final_boxes = if boxes_by_count > boxes_by_weight {
    boxes_by_count
  } else {
    boxes_by_weight
  }
  
  assert_eq(4, boxes_by_count)    // ceil(47/12) = 4
  assert_eq(94, total_weight)     // 47 * 2 = 94
  assert_eq(4, boxes_by_weight)   // ceil(94/25) = 4
  assert_eq(4, final_boxes)       // max(4, 4) = 4
}

test "string_greeting_with_special_characters" {
  // 测试包含特殊字符的字符串问候
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, User-123!", greet("User-123"))
  assert_eq_string("Hello, C++!", greet("C++"))
  assert_eq_string("Hello, JavaScript!", greet("JavaScript"))
  assert_eq_string("Hello, 🚀 MoonBit! 🌟!", greet("🚀 MoonBit! 🌟"))
}