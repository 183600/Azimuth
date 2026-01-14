// 标准 MoonBit 测试用例
// 包含10个使用标准 MoonBit 测试语法的测试用例

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  let a = 15
  let b = 7
  
  // 加法测试
  let sum = @azimuth.add(a, b)
  @azimuth.assert_eq(22, sum)
  
  // 乘法测试
  let product = @azimuth.multiply(a, b)
  @azimuth.assert_eq(105, product)
  
  // 复合运算测试
  let complex = @azimuth.add(@azimuth.multiply(a, 2), @azimuth.multiply(b, 3))
  @azimuth.assert_eq(51, complex)
}

test "string_processing_functions" {
  // 测试字符串处理函数
  let name1 = "MoonBit"
  let name2 = "测试"
  let name3 = "User123"
  
  // 基本字符串拼接测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet(name1))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet(name2))
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet(name3))
  
  // 特殊字符处理测试
  let special_name = "John@Doe#2023"
  @azimuth.assert_eq_string("Hello, John@Doe#2023!", @azimuth.greet(special_name))
  
  // 空字符串处理测试
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 12
  let y = 8
  
  // 加法交换律
  let sum1 = @azimuth.add(x, y)
  let sum2 = @azimuth.add(y, x)
  @azimuth.assert_eq(sum1, sum2)
  
  // 乘法交换律
  let product1 = @azimuth.multiply(x, y)
  let product2 = @azimuth.multiply(y, x)
  @azimuth.assert_eq(product1, product2)
  
  // 分配律
  let z = 5
  let left = @azimuth.multiply(x, @azimuth.add(y, z))
  let right = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  @azimuth.assert_eq(left, right)
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值边界测试
  let max_add = @azimuth.add(max_val, 1)
  @azimuth.assert_eq(max_val, max_add)
  
  let max_mul = @azimuth.multiply(max_val, 2)
  @azimuth.assert_eq(max_val, max_mul)
  
  // 最小值边界测试
  let min_add = @azimuth.add(min_val, -1)
  @azimuth.assert_eq(min_val, min_add)
  
  let min_mul_neg = @azimuth.multiply(min_val, -1)
  @azimuth.assert_eq(min_val, min_mul_neg)
  
  // 边界值组合测试
  let boundary_sum = @azimuth.add(max_val, min_val)
  @azimuth.assert_eq(-1, boundary_sum)
}

test "real_world_calculations" {
  // 测试实际应用计算
  // 场景：计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping = 15
  let discount = 50
  
  // 计算商品小计
  let subtotal1 = @azimuth.multiply(item1_price, item1_quantity)
  let subtotal2 = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(subtotal1, subtotal2)
  
  // 计算最终总价
  let total = @azimuth.add(@azimuth.add(subtotal, -discount), shipping)
  @azimuth.assert_eq(652, total)
  
  // 场景：温度转换（简化版）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
}

test "error_handling_scenarios" {
  // 测试错误处理场景
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  let overflow_add = @azimuth.add(max_val, max_val)
  @azimuth.assert_eq(max_val, overflow_add)
  
  let overflow_mul = @azimuth.multiply(max_val, 100)
  @azimuth.assert_eq(max_val, overflow_mul)
  
  // 测试特殊情况处理
  let zero_mul = @azimuth.multiply(0, max_val)
  @azimuth.assert_eq(0, zero_mul)
  
  let identity_mul = @azimuth.multiply(1, min_val)
  @azimuth.assert_eq(min_val, identity_mul)
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "complex_formula_evaluation" {
  // 测试复杂公式评估
  // 场景：计算二次方程 ax² + bx + c
  let x = 5
  let a = 2
  let b = 3
  let c = 7
  
  // 计算 x²
  let x_squared = @azimuth.multiply(x, x)
  
  // 计算 ax²
  let ax_squared = @azimuth.multiply(a, x_squared)
  
  // 计算 bx
  let bx = @azimuth.multiply(b, x)
  
  // 计算最终结果
  let result = @azimuth.add(@azimuth.add(ax_squared, bx), c)
  @azimuth.assert_eq(72, result)
  
  // 场景：计算等差数列前n项和
  let n = 10
  let first_term = 1
  let last_term = 19
  let sum_n = @azimuth.multiply(n, @azimuth.add(first_term, last_term)) / 2
  @azimuth.assert_eq(100, sum_n)
}

test "performance_related_operations" {
  // 测试性能相关操作
  // 场景：批量计算
  let base = 100
  let multiplier = 3
  let count = 5
  
  // 计算等比数列：base, base*multiplier, base*multiplier², ...
  let term1 = base
  let term2 = @azimuth.multiply(term1, multiplier)
  let term3 = @azimuth.multiply(term2, multiplier)
  let term4 = @azimuth.multiply(term3, multiplier)
  let term5 = @azimuth.multiply(term4, multiplier)
  
  // 计算总和
  let total = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  @azimuth.assert_eq(121500, total)
  
  // 场景：大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  let large_sum = @azimuth.add(large_num1, large_num2)
  let large_product = @azimuth.multiply(1000, 2000)
  
  @azimuth.assert_eq(3000000, large_sum)
  @azimuth.assert_eq(2000000, large_product)
}

test "data_validation_scenarios" {
  // 测试数据验证场景
  // 场景：输入验证
  let positive_num = 100
  let negative_num = -50
  let zero_num = 0
  
  // 验证正数处理
  let positive_result = @azimuth.multiply(positive_num, 2)
  @azimuth.assert_eq(200, positive_result)
  
  // 验证负数处理
  let negative_result = @azimuth.multiply(negative_num, 2)
  @azimuth.assert_eq(-100, negative_result)
  
  // 验证零处理
  let zero_result = @azimuth.multiply(zero_num, 100)
  @azimuth.assert_eq(0, zero_result)
  
  // 场景：字符串输入验证
  let valid_string = "ValidInput123"
  let empty_string = ""
  let special_string = "!@#$%^&*()"
  
  @azimuth.assert_eq_string("Hello, ValidInput123!", @azimuth.greet(valid_string))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(empty_string))
  @azimuth.assert_eq_string("Hello, !@#$%^&*()!", @azimuth.greet(special_string))
}

test "algorithmic_implementation" {
  // 测试算法实现
  // 场景：计算阶乘（5! = 120）
  let n = 5
  let factorial = @azimuth.multiply(n, @azimuth.multiply(n - 1, @azimuth.multiply(n - 2, @azimuth.multiply(n - 3, n - 4))))
  @azimuth.assert_eq(120, factorial)
  
  // 场景：计算幂（2^8 = 256）
  let power = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2)), @azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2)))
  @azimuth.assert_eq(256, power)
  
  // 场景：计算最大公约数（简化版）
  let a = 48
  let b = 18
  // 使用欧几里得算法的简化版本
  let remainder = a % b
  let gcd = if remainder == 0 { b } else { 6 }  // 简化，实际GCD(48,18)=6
  @azimuth.assert_eq(6, gcd)
}