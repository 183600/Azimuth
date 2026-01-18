// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，包含实用场景测试

// 定义所需的函数
pub fn add(a : Int, b : Int) -> Int {
  a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  a * b
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

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  let a = 15
  let b = 7
  
  // 测试加法
  let add_result = add(a, b)
  assert_eq(22, add_result)
  
  // 测试乘法
  let mul_result = multiply(a, b)
  assert_eq(105, mul_result)
  
  // 测试负数运算
  let negative_a = -10
  let negative_b = 5
  let add_negative = add(negative_a, negative_b)
  assert_eq(-5, add_negative)
  
  let mul_negative = multiply(negative_a, negative_b)
  assert_eq(-50, mul_negative)
}

test "zero_operations" {
  // 零值运算测试
  let positive = 42
  let negative = -17
  
  // 测试与零的加法
  let add_zero_positive = add(positive, 0)
  assert_eq(42, add_zero_positive)
  
  let add_zero_negative = add(negative, 0)
  assert_eq(-17, add_zero_negative)
  
  // 测试与零的乘法
  let mul_zero_positive = multiply(positive, 0)
  assert_eq(0, mul_zero_positive)
  
  let mul_zero_negative = multiply(negative, 0)
  assert_eq(0, mul_zero_negative)
}

test "greeting_functionality" {
  // 问候功能测试
  let name1 = "Alice"
  let name2 = "Bob"
  let name3 = "Charlie"
  let empty_name = ""
  
  // 测试基本问候
  let greeting1 = greet(name1)
  assert_eq_string("Hello, Alice!", greeting1)
  
  let greeting2 = greet(name2)
  assert_eq_string("Hello, Bob!", greeting2)
  
  let greeting3 = greet(name3)
  assert_eq_string("Hello, Charlie!", greeting3)
  
  // 测试空名称
  let greeting_empty = greet(empty_name)
  assert_eq_string("Hello, !", greeting_empty)
}

test "complex_calculations" {
  // 复杂计算测试
  let x = 5
  let y = 3
  let z = 2
  
  // 计算表达式：(x + y) * z
  let expr1 = multiply(add(x, y), z)
  assert_eq(16, expr1)
  
  // 计算表达式：x * y + z
  let expr2 = add(multiply(x, y), z)
  assert_eq(17, expr2)
  
  // 计算表达式：(x - y) * (x + y)
  let expr3 = multiply(add(x, -y), add(x, y))
  assert_eq(16, expr3)
}

test "sequence_operations" {
  // 序列运算测试
  let base = 10
  
  // 计算等差数列：base + (base+2) + (base+4) + (base+6) + (base+8)
  let term1 = base
  let term2 = add(base, 2)
  let term3 = add(base, 4)
  let term4 = add(base, 6)
  let term5 = add(base, 8)
  
  // 计算等差数列：base + (base+2) + (base+4) + (base+6) + (base+8)
  let sum1 = add(term1, term2)
  let sum2 = add(sum1, term3)
  let sum3 = add(sum2, term4)
  let sequence_sum = add(sum3, term5)
  assert_eq(60, sequence_sum)
  
  // 计算等比数列：base + 2*base + 4*base + 8*base + 16*base
  let geo_term1 = base
  let geo_term2 = multiply(2, base)
  let geo_term3 = multiply(4, base)
  let geo_term4 = multiply(8, base)
  let geo_term5 = multiply(16, base)
  
  let geo_sum1 = add(geo_term1, geo_term2)
  let geo_sum2 = add(geo_sum1, geo_term3)
  let geo_sum3 = add(geo_sum2, geo_term4)
  let geo_sum = add(geo_sum3, geo_term5)
  assert_eq(310, geo_sum)
}

test "boundary_value_testing" {
  // 边界值测试
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试最大值运算
  let max_add_1 = add(max_int, 0)
  assert_eq(max_int, max_add_1)
  
  let max_mul_1 = multiply(max_int, 1)
  assert_eq(max_int, max_mul_1)
  
  // 测试最小值运算
  let min_add_1 = add(min_int, 0)
  assert_eq(min_int, min_add_1)
  
  let min_mul_1 = multiply(min_int, 1)
  assert_eq(min_int, min_mul_1)
}

test "real_world_scenarios" {
  // 现实场景测试
  
  // 场景1：计算购物折扣
  let original_price = 100
  let discount_percentage = 20
  let discount_amount = multiply(original_price, discount_percentage) / 100
  let final_price = add(original_price, -discount_amount)
  assert_eq(80, final_price)
  
  // 场景2：计算BMI（简化版）
  let weight = 70  // kg
  let height_squared = multiply(175, 175) / 10000  // 1.75m²
  let bmi = multiply(weight, 10000) / multiply(175, 175)
  assert_eq(22, bmi)  // 70/(1.75²) ≈ 22
  // 使用 height_squared 变量避免警告
  let _ = height_squared
  
  // 场景3：计算汇率转换
  let usd_amount = 100
  let exchange_rate = 7  // 1 USD = 7 CNY
  let cny_amount = multiply(usd_amount, exchange_rate)
  assert_eq(700, cny_amount)
}

test "error_handling_scenarios" {
  // 错误处理场景测试
  let normal_value = 50
  
  // 测试正常运算
  let normal_add = add(normal_value, 10)
  assert_eq(60, normal_add)
  
  let normal_mul = multiply(normal_value, 2)
  assert_eq(100, normal_mul)
  
  // 测试大数运算（但不溢出）
  let large_value = 1000000
  let large_add = add(large_value, 1000000)
  assert_eq(2000000, large_add)
  
  let large_mul = multiply(large_value, 2)
  assert_eq(2000000, large_mul)
}

test "performance_patterns" {
  // 性能模式测试
  let base = 100
  
  // 测试重复加法 vs 乘法
  let repeated_add = add(add(add(base, base), base), base)
  let mul_equivalent = multiply(base, 4)
  assert_eq(repeated_add, mul_equivalent)
  
  // 测试嵌套运算
  let nested1 = multiply(add(10, 20), add(30, 40))
  let nested2 = add(multiply(10, 30), add(multiply(10, 40), add(multiply(20, 30), multiply(20, 40))))
  assert_eq(nested1, nested2)
  
  // 测试运算顺序
  let order1 = add(multiply(10, 20), 30)
  let order2 = multiply(10, add(20, 30))
  assert_eq(230, order1)
  assert_eq(500, order2)
}

test "mathematical_identities" {
  // 数学恒等式测试
  let a = 15
  let b = 7
  let c = 3
  
  // 测试加法恒等式
  let identity_add = add(a, 0)
  assert_eq(a, identity_add)
  
  // 测试乘法恒等式
  let identity_mul = multiply(a, 1)
  assert_eq(a, identity_mul)
  
  // 测试分配律：a × (b + c) = a × b + a × c
  let distributive1 = multiply(a, add(b, c))
  let distributive2 = add(multiply(a, b), multiply(a, c))
  assert_eq(distributive1, distributive2)
  
  // 测试结合律：(a + b) + c = a + (b + c)
  let associative_add1 = add(add(a, b), c)
  let associative_add2 = add(a, add(b, c))
  assert_eq(associative_add1, associative_add2)
  
  // 测试交换律：a + b = b + a
  let commutative_add1 = add(a, b)
  let commutative_add2 = add(b, a)
  assert_eq(commutative_add1, commutative_add2)
}