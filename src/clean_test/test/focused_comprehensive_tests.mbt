test "basic_math_operations" {
  let a = 10
  let b = 20
  
  let sum = @clean_test.add(a, b)
  let product = @clean_test.multiply(a, b)
  
  @clean_test.assert_eq(30, sum)
  @clean_test.assert_eq(200, product)
}

// 测试用例 2: 乘法分配律
///|
test "multiplication_distributive_law" {
  let a = 2
  let b = 3
  let c = 5
  
  let left_side = @clean_test.multiply(a, @clean_test.add(b, c))
  let right_side = @clean_test.add(@clean_test.multiply(a, b), @clean_test.multiply(a, c))
  
  @clean_test.assert_eq(left_side, right_side)
  
  let x = -2
  let y = 3
  let z = -5
  
  let left_side2 = @clean_test.multiply(x, @clean_test.add(y, z))
  let right_side2 = @clean_test.add(@clean_test.multiply(x, y), @clean_test.multiply(x, z))
  
  @clean_test.assert_eq(left_side2, right_side2)
}

// 测试用例 3: 购物车计算
///|
test "shopping_cart_calculation" {
  let item1_price = 100
  let item2_price = 200
  let item3_price = 300
  
  let item1_qty = 2
  let item2_qty = 1
  let item3_qty = 3
  
  let subtotal1 = @clean_test.multiply(item1_price, item1_qty)
  let subtotal2 = @clean_test.multiply(item2_price, item2_qty)
  let subtotal3 = @clean_test.multiply(item3_price, item3_qty)
  
  let total = @clean_test.add(@clean_test.add(subtotal1, subtotal2), subtotal3)
  
  @clean_test.assert_eq(100 * 2 + 200 + 300 * 3, total)
}

// 测试用例 4: 折扣计算
///|
test "discount_calculation" {
  let original_price = 1000
  let discount_percent = 20
  
  let discount_amount = @clean_test.multiply(original_price, discount_percent)
  let final_price = @clean_test.add(original_price, @clean_test.multiply(-1, discount_amount))
  
  @clean_test.assert_eq(800, final_price)
}

// 测试用例 5: 字符串处理
///|
test "string_processing" {
  let name1 = "Alice"
  let name2 = "Bob"
  
  let greeting1 = @clean_test.greet(name1)
  let greeting2 = @clean_test.greet(name2)
  
  @clean_test.assert_eq_string("Hello, Alice!", greeting1)
  @clean_test.assert_eq_string("Hello, Bob!", greeting2)
}
