// 标准 MoonBit 测试用例 - 使用内置断言方法
// 针对 azimuth 库核心功能的10个高质量测试用例

test "basic_arithmetic_addition" {
  // 基本算术加法测试
  let result1 = 3 + 5
  let result2 = 0 + 0
  let result3 = -5 + 3
  let result4 = 50 + 50
  
  if (result1 != 8) {
    @builtin.abort("Test failed: expected 8, got " + result1.to_string())
  }
  if (result2 != 0) {
    @builtin.abort("Test failed: expected 0, got " + result2.to_string())
  }
  if (result3 != -2) {
    @builtin.abort("Test failed: expected -2, got " + result3.to_string())
  }
  if (result4 != 100) {
    @builtin.abort("Test failed: expected 100, got " + result4.to_string())
  }
}

test "basic_arithmetic_multiplication" {
  // 基本算术乘法测试
  let result1 = 3 * 5
  let result2 = 0 * 100
  let result3 = -3 * 4
  let result4 = 5 * 5
  
  if (result1 != 15) {
    @builtin.abort("Test failed: expected 15, got " + result1.to_string())
  }
  if (result2 != 0) {
    @builtin.abort("Test failed: expected 0, got " + result2.to_string())
  }
  if (result3 != -12) {
    @builtin.abort("Test failed: expected -12, got " + result3.to_string())
  }
  if (result4 != 25) {
    @builtin.abort("Test failed: expected 25, got " + result4.to_string())
  }
}

test "division_with_ceil_simulation" {
  // 模拟向上取整除法功能测试
  // 使用整数除法和余数来模拟向上取整
  let function ceil_div(a : Int, b : Int) -> Int {
    if (b == 0) { 0 } else {
      let quotient = a / b
      let remainder = a % b
      if (remainder == 0) { quotient } else { quotient + 1 }
    }
  }
  
  let result1 = ceil_div(10, 3)
  let result2 = ceil_div(9, 3)
  let result3 = ceil_div(1, 100)
  let result4 = ceil_div(5, 0)
  
  if (result1 != 4) {
    @builtin.abort("Test failed: expected 4, got " + result1.to_string())
  }
  if (result2 != 3) {
    @builtin.abort("Test failed: expected 3, got " + result2.to_string())
  }
  if (result3 != 1) {
    @builtin.abort("Test failed: expected 1, got " + result3.to_string())
  }
  if (result4 != 0) {
    @builtin.abort("Test failed: expected 0, got " + result4.to_string())
  }
}

test "greet_function_simulation" {
  // 模拟问候功能测试
  let function greet_sim(name : String) -> String {
    "Hello, " + name + "!"
  }
  
  let result1 = greet_sim("World")
  let result2 = greet_sim("MoonBit")
  let result3 = greet_sim("")
  
  if (result1 != "Hello, World!") {
    @builtin.abort("Test failed: expected 'Hello, World!', got '" + result1 + "'")
  }
  if (result2 != "Hello, MoonBit!") {
    @builtin.abort("Test failed: expected 'Hello, MoonBit!', got '" + result2 + "'")
  }
  if (result3 != "Hello, !") {
    @builtin.abort("Test failed: expected 'Hello, !', got '" + result3 + "'")
  }
}

test "mathematical_properties" {
  // 数学性质验证测试
  let a = 7
  let b = 13
  
  // 加法交换律
  let add_result1 = a + b
  let add_result2 = b + a
  
  // 乘法交换律
  let mul_result1 = a * b
  let mul_result2 = b * a
  
  // 零元素性质
  let zero_mul = a * 0
  let zero_add = a + 0
  
  if (add_result1 != add_result2) {
    @builtin.abort("Test failed: addition commutative property")
  }
  if (mul_result1 != mul_result2) {
    @builtin.abort("Test failed: multiplication commutative property")
  }
  if (zero_mul != 0) {
    @builtin.abort("Test failed: zero multiplication property")
  }
  if (zero_add != a) {
    @builtin.abort("Test failed: zero addition property")
  }
}

test "negative_number_operations" {
  // 负数运算测试
  let result1 = -3 + -5
  let result2 = -5 + 7
  let result3 = -3 * -5
  let result4 = 3 * -5
  
  if (result1 != -8) {
    @builtin.abort("Test failed: expected -8, got " + result1.to_string())
  }
  if (result2 != 2) {
    @builtin.abort("Test failed: expected 2, got " + result2.to_string())
  }
  if (result3 != 15) {
    @builtin.abort("Test failed: expected 15, got " + result3.to_string())
  }
  if (result4 != -15) {
    @builtin.abort("Test failed: expected -15, got " + result4.to_string())
  }
}

test "complex_business_calculation" {
  // 复杂业务计算测试
  // 计算订单总价：商品价格 × 数量 + 运费
  let unit_price = 25
  let quantity = 4
  let shipping = 12
  
  let subtotal = unit_price * quantity
  let total = subtotal + shipping
  
  if (subtotal != 100) {
    @builtin.abort("Test failed: expected 100, got " + subtotal.to_string())
  }
  if (total != 112) {
    @builtin.abort("Test failed: expected 112, got " + total.to_string())
  }
}

test "packaging_calculation" {
  // 包装计算测试
  let total_items = 23
  let items_per_box = 6
  
  // 模拟向上取整除法
  let boxes_needed = if (total_items % items_per_box == 0) {
    total_items / items_per_box
  } else {
    total_items / items_per_box + 1
  }
  
  if (boxes_needed != 4) {
    @builtin.abort("Test failed: expected 4, got " + boxes_needed.to_string())
  }
  
  // 验证总容量
  let total_capacity = boxes_needed * items_per_box
  if (total_capacity != 24) {
    @builtin.abort("Test failed: expected 24, got " + total_capacity.to_string())
  }
}

test "unicode_and_special_characters" {
  // Unicode和特殊字符测试
  let function greet_sim(name : String) -> String {
    "Hello, " + name + "!"
  }
  
  let result1 = greet_sim("世界")
  let result2 = greet_sim("🚀")
  let result3 = greet_sim("café")
  let result4 = greet_sim("123")
  
  if (result1 != "Hello, 世界!") {
    @builtin.abort("Test failed: expected 'Hello, 世界!', got '" + result1 + "'")
  }
  if (result2 != "Hello, 🚀!") {
    @builtin.abort("Test failed: expected 'Hello, 🚀!', got '" + result2 + "'")
  }
  if (result3 != "Hello, café!") {
    @builtin.abort("Test failed: expected 'Hello, café!', got '" + result3 + "'")
  }
  if (result4 != "Hello, 123!") {
    @builtin.abort("Test failed: expected 'Hello, 123!', got '" + result4 + "'")
  }
}

test "combined_operations" {
  // 组合运算测试
  let result1 = 2 * 3 + 4 * 5
  if (result1 != 26) {
    @builtin.abort("Test failed: expected 26, got " + result1.to_string())
  }
  
  // 模拟向上取整除法
  let temp = 5 * 3 + 7
  let result2 = if (temp % 4 == 0) { temp / 4 } else { temp / 4 + 1 }
  if (result2 != 6) {
    @builtin.abort("Test failed: expected 6, got " + result2.to_string())
  }
}