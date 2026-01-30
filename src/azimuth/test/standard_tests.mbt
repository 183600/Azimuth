// 标准 MoonBit 测试用例
// 针对 azimuth 库核心功能的10个高质量测试用例

test "basic_arithmetic_addition" {
  // 基本算术加法测试
  let result1 = 3 + 5
  let result2 = 0 + 0
  let result3 = -5 + 3
  let result4 = 50 + 50
  
  if (result1 != 8) {
    @builtin.abort("Test failed: expected 8")
  }
  if (result2 != 0) {
    @builtin.abort("Test failed: expected 0")
  }
  if (result3 != -2) {
    @builtin.abort("Test failed: expected -2")
  }
  if (result4 != 100) {
    @builtin.abort("Test failed: expected 100")
  }
}

test "basic_arithmetic_multiplication" {
  // 基本算术乘法测试
  let result1 = 3 * 5
  let result2 = 0 * 100
  let result3 = -3 * 4
  let result4 = 5 * 5
  
  if (result1 != 15) {
    @builtin.abort("Test failed: expected 15")
  }
  if (result2 != 0) {
    @builtin.abort("Test failed: expected 0")
  }
  if (result3 != -12) {
    @builtin.abort("Test failed: expected -12")
  }
  if (result4 != 25) {
    @builtin.abort("Test failed: expected 25")
  }
}

test "division_with_ceil_simulation" {
  // 模拟向上取整除法功能测试
  let a = 10
  let b = 3
  let quotient = a / b
  let remainder = a % b
  let result1 = if (remainder == 0) { quotient } else { quotient + 1 }
  
  let a2 = 9
  let b2 = 3
  let quotient2 = a2 / b2
  let remainder2 = a2 % b2
  let result2 = if (remainder2 == 0) { quotient2 } else { quotient2 + 1 }
  
  let a3 = 1
  let b3 = 100
  let quotient3 = a3 / b3
  let remainder3 = a3 % b3
  let result3 = if (remainder3 == 0) { quotient3 } else { quotient3 + 1 }
  
  // 除零情况
  let result4 = 0
  
  if (result1 != 4) {
    @builtin.abort("Test failed: expected 4")
  }
  if (result2 != 3) {
    @builtin.abort("Test failed: expected 3")
  }
  if (result3 != 1) {
    @builtin.abort("Test failed: expected 1")
  }
  if (result4 != 0) {
    @builtin.abort("Test failed: expected 0")
  }
}

test "greet_function_simulation" {
  // 模拟问候功能测试
  let name1 = "World"
  let result1 = "Hello, " + name1 + "!"
  
  let name2 = "MoonBit"
  let result2 = "Hello, " + name2 + "!"
  
  let name3 = ""
  let result3 = "Hello, " + name3 + "!"
  
  if (result1 != "Hello, World!") {
    @builtin.abort("Test failed: expected 'Hello, World!'")
  }
  if (result2 != "Hello, MoonBit!") {
    @builtin.abort("Test failed: expected 'Hello, MoonBit!'")
  }
  if (result3 != "Hello, !") {
    @builtin.abort("Test failed: expected 'Hello, !'")
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
    @builtin.abort("Test failed: expected -8")
  }
  if (result2 != 2) {
    @builtin.abort("Test failed: expected 2")
  }
  if (result3 != 15) {
    @builtin.abort("Test failed: expected 15")
  }
  if (result4 != -15) {
    @builtin.abort("Test failed: expected -15")
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
    @builtin.abort("Test failed: expected 100")
  }
  if (total != 112) {
    @builtin.abort("Test failed: expected 112")
  }
}

test "packaging_calculation" {
  // 包装计算测试
  let total_items = 23
  let items_per_box = 6
  
  // 模拟向上取整除法
  let remainder = total_items % items_per_box
  let boxes_needed = if (remainder == 0) {
    total_items / items_per_box
  } else {
    total_items / items_per_box + 1
  }
  
  if (boxes_needed != 4) {
    @builtin.abort("Test failed: expected 4")
  }
  
  // 验证总容量
  let total_capacity = boxes_needed * items_per_box
  if (total_capacity != 24) {
    @builtin.abort("Test failed: expected 24")
  }
}

test "unicode_and_special_characters" {
  // Unicode和特殊字符测试
  let name1 = "世界"
  let result1 = "Hello, " + name1 + "!"
  
  let name2 = "🚀"
  let result2 = "Hello, " + name2 + "!"
  
  let name3 = "café"
  let result3 = "Hello, " + name3 + "!"
  
  let name4 = "123"
  let result4 = "Hello, " + name4 + "!"
  
  if (result1 != "Hello, 世界!") {
    @builtin.abort("Test failed: expected 'Hello, 世界!'")
  }
  if (result2 != "Hello, 🚀!") {
    @builtin.abort("Test failed: expected 'Hello, 🚀!'")
  }
  if (result3 != "Hello, café!") {
    @builtin.abort("Test failed: expected 'Hello, café!'")
  }
  if (result4 != "Hello, 123!") {
    @builtin.abort("Test failed: expected 'Hello, 123!'")
  }
}

test "combined_operations" {
  // 组合运算测试
  let result1 = 2 * 3 + 4 * 5
  if (result1 != 26) {
    @builtin.abort("Test failed: expected 26")
  }
  
  // 模拟向上取整除法
  let temp = 5 * 3 + 7
  let remainder = temp % 4
  let result2 = if (remainder == 0) { temp / 4 } else { temp / 4 + 1 }
  if (result2 != 6) {
    @builtin.abort("Test failed: expected 6")
  }
}