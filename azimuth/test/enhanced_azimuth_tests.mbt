// 增强的 Azimuth 测试用例
// 使用标准的 MoonBit 测试语法

test "add_large_numbers" {
  // 测试大数相加
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 接近最大值的加法
  @azimuth.assert_eq(max_val, @azimuth.add(1000000000, 1147483647))
  @azimuth.assert_eq(max_val, @azimuth.add(2000000000, 147483647))
  
  // 接近最小值的加法
  @azimuth.assert_eq(min_val, @azimuth.add(-1000000000, -1147483648))
  @azimuth.assert_eq(min_val, @azimuth.add(-2000000000, -147483648))
  
  // 对称性测试
  @azimuth.assert_eq(@azimuth.add(123456789, 987654321), @azimuth.add(987654321, 123456789))
}

test "multiply_large_numbers" {
  // 测试大数相乘
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 大数相乘但不溢出
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(99980001, @azimuth.multiply(9999, 9999))
  
  // 溢出测试
  @azimuth.assert_eq(max_val, @azimuth.multiply(50000, 50000))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-50000, 50000))
  
  // 特殊值测试
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "greet_complex_strings" {
  // 测试复杂字符串处理
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, line1!", @azimuth.greet("line1"))
  @azimuth.assert_eq_string("Hello, spaced!", @azimuth.greet("spaced"))
  @azimuth.assert_eq_string("Hello, quoted!", @azimuth.greet("quoted"))
}

test "complex_arithmetic_expressions" {
  // 测试复合算术表达式
  let x = 15
  let y = 25
  let z = -10
  
  // (x + y) * z
  let sum = @azimuth.add(x, y)
  let result1 = @azimuth.multiply(sum, z)
  @azimuth.assert_eq(-400, result1)
  
  // x * y + z
  let product = @azimuth.multiply(x, y)
  let result2 = @azimuth.add(product, z)
  @azimuth.assert_eq(365, result2)
  
  // 嵌套运算
  let temp = @azimuth.multiply(x, z)
  let result3 = @azimuth.add(temp, @azimuth.multiply(y, z))
  @azimuth.assert_eq(-400, result3)
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法边界
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1000000000))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1000000000))
  
  // 乘法边界
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(max_val, @azimuth.multiply(65536, 65536))
}

test "mathematical_properties" {
  // 测试数学性质
  
  // 加法交换律
  let a = 123
  let b = 456
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 加法结合律（近似测试）
  let c = 789
  let left = @azimuth.add(@azimuth.add(a, b), c)
  let right = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left, right)
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律（近似测试）
  let distrib_left = @azimuth.multiply(a, @azimuth.add(b, c))
  let distrib_right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(distrib_left, distrib_right)
  
  // 零元素性质
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  
  // 单位元素性质
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
}

test "special_input_cases" {
  // 测试特殊输入情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 连续运算测试
  let result1 = @azimuth.add(1, 1)
  let result2 = @azimuth.add(result1, 1)
  let result3 = @azimuth.add(result2, 1)
  @azimuth.assert_eq(4, result3)
  
  // 极值与普通值的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 负数运算
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
  @azimuth.assert_eq(-200, @azimuth.multiply(100, -2))
  
  // 字符串边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
}

test "function_composition" {
  // 测试函数组合
  
  // 计算数列的和
  let sum1 = @azimuth.add(1, 2)
  let sum2 = @azimuth.add(sum1, 3)
  let sum3 = @azimuth.add(sum2, 4)
  let total = @azimuth.add(sum3, 5)
  @azimuth.assert_eq(15, total)
  
  // 计算阶乘（简化版）
  let fact2 = @azimuth.multiply(2, 1)
  let fact3 = @azimuth.multiply(3, fact2)
  let fact4 = @azimuth.multiply(4, fact3)
  @azimuth.assert_eq(24, fact4)
  
  // 字符串处理链
  let greeting1 = @azimuth.greet("User")
  let greeting2 = @azimuth.greet("Admin")
  @azimuth.assert_eq_string("Hello, User!", greeting1)
  @azimuth.assert_eq_string("Hello, Admin!", greeting2)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  
  // 购物车计算
  let item1_price = 199
  let item2_price = 299
  let item3_price = 399
  let discount = -50
  
  let subtotal = @azimuth.add(item1_price, item2_price)
  let subtotal2 = @azimuth.add(subtotal, item3_price)
  let total = @azimuth.add(subtotal2, discount)
  @azimuth.assert_eq(847, total)
  
  // 温度转换（简化）
  let celsius = 25
  let fahrenheit = @azimuth.multiply(@azimuth.multiply(celsius, 9), 5)  // 这里简化了除法
  @azimuth.assert_eq(225, fahrenheit)  // 简化结果
  
  // 用户个性化问候
  let user_name = "张三"
  let admin_name = "Admin"
  let user_greeting = @azimuth.greet(user_name)
  let admin_greeting = @azimuth.greet(admin_name)
  @azimuth.assert_eq_string("Hello, 张三!", user_greeting)
  @azimuth.assert_eq_string("Hello, Admin!", admin_greeting)
}

test "performance_related_operations" {
  // 测试性能相关操作
  
  // 重复运算测试
  let base = 1000
  let result1 = @azimuth.add(base, base)
  let result2 = @azimuth.add(result1, base)
  let result3 = @azimuth.add(result2, base)
  let result4 = @azimuth.add(result3, base)
  @azimuth.assert_eq(5000, result4)
  
  // 连续乘法测试
  let mult_base = 2
  let mult_result1 = @azimuth.multiply(mult_base, 2)
  let mult_result2 = @azimuth.multiply(mult_result1, 2)
  let mult_result3 = @azimuth.multiply(mult_result2, 2)
  let mult_result4 = @azimuth.multiply(mult_result3, 2)
  @azimuth.assert_eq(32, mult_result4)
  
  // 混合运算压力测试
  let mixed = @azimuth.add(100, 200)
  mixed = @azimuth.multiply(mixed, 3)
  mixed = @azimuth.add(mixed, 150)
  mixed = @azimuth.multiply(mixed, 2)
  @azimuth.assert_eq(2100, mixed)
  
  // 字符串处理性能
  let long_name = "VeryLongUserName"
  let long_greeting = @azimuth.greet(long_name)
  @azimuth.assert_eq_string("Hello, VeryLongUserName!", long_greeting)
}