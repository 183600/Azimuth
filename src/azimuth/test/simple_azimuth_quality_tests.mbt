// 简单的 MoonBit 测试用例 - 使用内置断言
// 包含基本数学运算和字符串处理的测试

test "basic_addition_operations" {
  // 测试基本加法运算
  let result1 = 2 + 3
  if (result1 != 5) {
    @builtin.abort("Test failed: 2 + 3 should equal 5")
  }
  
  let result2 = 10 + 15
  if (result2 != 25) {
    @builtin.abort("Test failed: 10 + 15 should equal 25")
  }
  
  let result3 = -5 + 8
  if (result3 != 3) {
    @builtin.abort("Test failed: -5 + 8 should equal 3")
  }
}

test "basic_multiplication_operations" {
  // 测试基本乘法运算
  let result1 = 4 * 5
  if (result1 != 20) {
    @builtin.abort("Test failed: 4 * 5 should equal 20")
  }
  
  let result2 = 7 * 8
  if (result2 != 56) {
    @builtin.abort("Test failed: 7 * 8 should equal 56")
  }
  
  let result3 = -3 * 6
  if (result3 != -18) {
    @builtin.abort("Test failed: -3 * 6 should equal -18")
  }
}

test "combined_arithmetic_operations" {
  // 测试组合算术运算
  let result1 = (2 + 3) * 4
  if (result1 != 20) {
    @builtin.abort("Test failed: (2 + 3) * 4 should equal 20")
  }
  
  let result2 = 10 * 3 + 5 * 2
  if (result2 != 40) {
    @builtin.abort("Test failed: 10 * 3 + 5 * 2 should equal 40")
  }
  
  let result3 = (15 - 5) * (8 / 2)
  if (result3 != 40) {
    @builtin.abort("Test failed: (15 - 5) * (8 / 2) should equal 40")
  }
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 12
  let b = 17
  
  // 加法交换律
  let add_ab = a + b
  let add_ba = b + a
  if (add_ab != add_ba) {
    @builtin.abort("Test failed: Addition should be commutative")
  }
  
  // 乘法交换律
  let mul_ab = a * b
  let mul_ba = b * a
  if (mul_ab != mul_ba) {
    @builtin.abort("Test failed: Multiplication should be commutative")
  }
}

test "mathematical_associativity" {
  // 测试数学结合律
  let a = 5
  let b = 7
  let c = 9
  
  // 加法结合律
  let add_abc = (a + b) + c
  let add_abc2 = a + (b + c)
  if (add_abc != add_abc2) {
    @builtin.abort("Test failed: Addition should be associative")
  }
  
  // 乘法结合律
  let mul_abc = (a * b) * c
  let mul_abc2 = a * (b * c)
  if (mul_abc != mul_abc2) {
    @builtin.abort("Test failed: Multiplication should be associative")
  }
}

test "distributive_property" {
  // 测试分配律
  let a = 4
  let b = 6
  let c = 8
  
  let left_side = a * (b + c)
  let right_side = a * b + a * c
  if (left_side != right_side) {
    @builtin.abort("Test failed: Multiplication should distribute over addition")
  }
}

test "division_with_ceil_simulation" {
  // 模拟向上取整除法
  // 使用整数除法和余数来模拟向上取整
  
  // 10 / 3 = 3.333... -> 4
  let a = 10
  let b = 3
  let quotient = a / b
  let remainder = a % b
  let ceil_result = if remainder == 0 { quotient } else { quotient + 1 }
  if (ceil_result != 4) {
    @builtin.abort("Test failed: ceil(10/3) should equal 4")
  }
  
  // 9 / 3 = 3 -> 3
  let c = 9
  let d = 3
  let quotient2 = c / d
  let remainder2 = c % d
  let ceil_result2 = if remainder2 == 0 { quotient2 } else { quotient2 + 1 }
  if (ceil_result2 != 3) {
    @builtin.abort("Test failed: ceil(9/3) should equal 3")
  }
  
  // 1 / 100 = 0.01 -> 1
  let e = 1
  let f = 100
  let quotient3 = e / f
  let remainder3 = e % f
  let ceil_result3 = if remainder3 == 0 { quotient3 } else { quotient3 + 1 }
  if (ceil_result3 != 1) {
    @builtin.abort("Test failed: ceil(1/100) should equal 1")
  }
}

test "string_concatenation_operations" {
  // 测试字符串连接操作
  let str1 = "Hello"
  let str2 = "World"
  let result1 = str1 + ", " + str2 + "!"
  if (result1 != "Hello, World!") {
    @builtin.abort("Test failed: String concatenation should work correctly")
  }
  
  let str3 = "MoonBit"
  let result2 = "Welcome to " + str3
  if (result2 != "Welcome to MoonBit") {
    @builtin.abort("Test failed: String concatenation with prefix should work correctly")
  }
  
  let str4 = ""
  let result3 = str4 + "Empty"
  if (result3 != "Empty") {
    @builtin.abort("Test failed: String concatenation with empty string should work correctly")
  }
}

test "business_calculation_scenario" {
  // 测试业务计算场景
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  // 计算小计
  let subtotal = unit_price * quantity
  
  // 计算税费（简化为固定金额）
  let tax_amount = subtotal * tax_rate / 100
  
  // 计算总价
  let total = subtotal + tax_amount + shipping
  
  if (subtotal != 200) {
    @builtin.abort("Test failed: Subtotal should be 200")
  }
  
  if (tax_amount != 16) {
    @builtin.abort("Test failed: Tax amount should be 16")
  }
  
  if (total != 231) {
    @builtin.abort("Test failed: Total should be 231")
  }
}

test "packaging_calculation_scenario" {
  // 测试包装计算场景
  let total_items = 25
  let items_per_box = 6
  
  // 计算需要的箱子数量（向上取整）
  let quotient = total_items / items_per_box
  let remainder = total_items % items_per_box
  let boxes_needed = if remainder == 0 { quotient } else { quotient + 1 }
  
  if (boxes_needed != 5) {
    @builtin.abort("Test failed: Boxes needed should be 5")
  }
}