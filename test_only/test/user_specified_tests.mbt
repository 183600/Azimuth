// 用户指定的 MoonBit 测试用例

// 测试用例1: 测试加法函数的零值特性
test "add_zero_properties" {
  // 测试加法的零值特性
  @test_only.assert_eq(42, @test_only.add(42, 0))
  @test_only.assert_eq(42, @test_only.add(0, 42))
  @test_only.assert_eq(0, @test_only.add(0, 0))
  @test_only.assert_eq(-17, @test_only.add(-17, 0))
  @test_only.assert_eq(-17, @test_only.add(0, -17))
}

// 测试用例2: 测试乘法函数的零值和单位元特性
test "multiply_identity_properties" {
  // 测试乘法的零值特性
  @test_only.assert_eq(0, @test_only.multiply(123, 0))
  @test_only.assert_eq(0, @test_only.multiply(0, 123))
  @test_only.assert_eq(0, @test_only.multiply(0, 0))
  
  // 测试乘法的单位元特性
  @test_only.assert_eq(123, @test_only.multiply(123, 1))
  @test_only.assert_eq(123, @test_only.multiply(1, 123))
  @test_only.assert_eq(-456, @test_only.multiply(-456, 1))
  @test_only.assert_eq(-456, @test_only.multiply(1, -456))
}

// 测试用例3: 测试负数乘法的符号规则
test "negative_multiplication_sign_rules" {
  // 正数 × 正数 = 正数
  @test_only.assert_eq(15, @test_only.multiply(3, 5))
  
  // 负数 × 负数 = 正数
  @test_only.assert_eq(15, @test_only.multiply(-3, -5))
  
  // 正数 × 负数 = 负数
  @test_only.assert_eq(-15, @test_only.multiply(3, -5))
  
  // 负数 × 正数 = 负数
  @test_only.assert_eq(-15, @test_only.multiply(-3, 5))
}

// 测试用例4: 测试加法结合律
test "addition_associativity" {
  let a = 12
  let b = 34
  let c = 56
  
  // (a + b) + c = a + (b + c)
  let left = @test_only.add(@test_only.add(a, b), c)
  let right = @test_only.add(a, @test_only.add(b, c))
  @test_only.assert_eq(left, right)
  
  // 使用不同的数值测试
  let x = -100
  let y = 200
  let z = -50
  
  let left2 = @test_only.add(@test_only.add(x, y), z)
  let right2 = @test_only.add(x, @test_only.add(y, z))
  @test_only.assert_eq(left2, right2)
}

// 测试用例5: 测试乘法结合律
test "multiplication_associativity" {
  let a = 2
  let b = 3
  let c = 4
  
  // (a × b) × c = a × (b × c)
  let left = @test_only.multiply(@test_only.multiply(a, b), c)
  let right = @test_only.multiply(a, @test_only.multiply(b, c))
  @test_only.assert_eq(left, right)
  
  // 使用负数测试
  let x = -2
  let y = 5
  let z = -3
  
  let left2 = @test_only.multiply(@test_only.multiply(x, y), z)
  let right2 = @test_only.multiply(x, @test_only.multiply(y, z))
  @test_only.assert_eq(left2, right2)
}

// 测试用例6: 测试字符串处理的特殊字符
test "greet_special_characters" {
  // 测试包含空格的字符串
  @test_only.assert_eq_string("Hello, John Doe!", @test_only.greet("John Doe"))
  
  // 测试包含数字的字符串
  @test_only.assert_eq_string("Hello, 2023!", @test_only.greet("2023"))
  
  // 测试包含特殊符号的字符串
  @test_only.assert_eq_string("Hello, @#$%!", @test_only.greet("@#$%"))
  
  // 测试包含混合字符的字符串
  @test_only.assert_eq_string("Hello, Test123!", @test_only.greet("Test123"))
}

// 测试用例7: 测试大数运算的边界情况
test "large_number_boundary_tests" {
  // 测试接近最大值的加法
  let near_max = 2147483640
  @test_only.assert_eq(2147483647, @test_only.add(near_max, 7))
  @test_only.assert_eq(2147483647, @test_only.add(7, near_max))
  
  // 测试接近最小值的加法
  let near_min = -2147483640
  @test_only.assert_eq(-2147483648, @test_only.add(near_min, -8))
  @test_only.assert_eq(-2147483648, @test_only.add(-8, near_min))
  
  // 测试大数乘法（但不溢出）
  @test_only.assert_eq(1000000, @test_only.multiply(1000, 1000))
  @test_only.assert_eq(-1000000, @test_only.multiply(1000, -1000))
}

// 测试用例8: 测试复合运算场景
test "compound_operation_scenarios" {
  // 计算正方形的面积和周长
  let side = 5
  let area = @test_only.multiply(side, side)
  let perimeter = @test_only.multiply(4, side)
  
  @test_only.assert_eq(25, area)
  @test_only.assert_eq(20, perimeter)
  
  // 计算平均数
  let num1 = 85
  let num2 = 90
  let num3 = 95
  let sum = @test_only.add(@test_only.add(num1, num2), num3)
  let average = @test_only.multiply(sum, 1)  // 这里简化为不除以3，因为测试中没有除法
  @test_only.assert_eq(270, average)
}

// 测试用例9: 测试数学恒等式
test "mathematical_identities" {
  // 测试加法交换律
  let a = 123
  let b = 456
  @test_only.assert_eq(@test_only.add(a, b), @test_only.add(b, a))
  
  // 测试乘法交换律
  @test_only.assert_eq(@test_only.multiply(a, b), @test_only.multiply(b, a))
  
  // 测试分配律 a × (b + c) = a × b + a × c
  let c = 789
  let left = @test_only.multiply(a, @test_only.add(b, c))
  let right = @test_only.add(@test_only.multiply(a, b), @test_only.multiply(a, c))
  @test_only.assert_eq(left, right)
}

// 测试用例10: 测试边界值的特殊处理
test "boundary_value_special_cases" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值加1
  @test_only.assert_eq(max_val, @test_only.add(max_val, 1))
  
  // 测试最小值减1
  @test_only.assert_eq(min_val, @test_only.add(min_val, -1))
  
  // 测试最大值和最小值相加
  @test_only.assert_eq(-1, @test_only.add(max_val, min_val))
  
  // 测试最小值与-1相乘
  @test_only.assert_eq(min_val, @test_only.multiply(min_val, -1))
}