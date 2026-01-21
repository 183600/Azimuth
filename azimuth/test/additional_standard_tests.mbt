// 额外的标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_function_basic_properties" {
  // 测试加法的基本性质
  // 交换律
  @azimuth.assert_eq(@azimuth.add(5, 3), @azimuth.add(3, 5))
  @azimuth.assert_eq(@azimuth.add(-2, 7), @azimuth.add(7, -2))
  
  // 结合律（在安全范围内）
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(2, 3), 4), @azimuth.add(2, @azimuth.add(3, 4)))
  
  // 单位元性质
  @azimuth.assert_eq(7, @azimuth.add(7, 0))
  @azimuth.assert_eq(7, @azimuth.add(0, 7))
  @azimuth.assert_eq(-5, @azimuth.add(-5, 0))
  @azimuth.assert_eq(-5, @azimuth.add(0, -5))
}

test "multiply_function_basic_properties" {
  // 测试乘法的基本性质
  // 交换律
  @azimuth.assert_eq(@azimuth.multiply(4, 6), @azimuth.multiply(6, 4))
  @azimuth.assert_eq(@azimuth.multiply(-3, 8), @azimuth.multiply(8, -3))
  
  // 结合律（在安全范围内）
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(2, 3), 4), @azimuth.multiply(2, @azimuth.multiply(3, 4)))
  
  // 单位元性质
  @azimuth.assert_eq(9, @azimuth.multiply(9, 1))
  @azimuth.assert_eq(9, @azimuth.multiply(1, 9))
  @azimuth.assert_eq(-7, @azimuth.multiply(-7, 1))
  @azimuth.assert_eq(-7, @azimuth.multiply(1, -7))
  
  // 零元性质
  @azimuth.assert_eq(0, @azimuth.multiply(9, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 9))
  @azimuth.assert_eq(0, @azimuth.multiply(-7, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -7))
}

test "distributive_property" {
  // 测试分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 3
  let c = 2
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)
  
  // 测试另一组数值
  let x = 4
  let y = -2
  let z = 7
  
  let left_side2 = @azimuth.multiply(x, @azimuth.add(y, z))
  let right_side2 = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  
  @azimuth.assert_eq(left_side2, right_side2)
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  // 普通文本
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
  
  // 数字字符串
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 456789!", @azimuth.greet("456789"))
  
  // 特殊字符
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
  @azimuth.assert_eq_string("Hello, ***!", @azimuth.greet("***"))
  
  // 空字符串和空格
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet(" "))
}

test "divide_with_ceil_practical_cases" {
  // 测试向上取整除法的实际应用场景
  // 分页计算
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(25, 10))  // 25项，每页10项，需要3页
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(41, 10))  // 41项，每页10项，需要5页
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 10))   // 5项，每页10项，需要1页
  
  // 容器计算
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(15, 4))   // 15个物品，每个容器装4个，需要4个容器
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(8, 5))    // 8个物品，每个容器装5个，需要2个容器
  
  // 负数情况
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-15, 8))  // 负数除以正数
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-15, -8))  // 负数除以负数
}

test "boundary_value_comprehensive" {
  // 综合边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值和最小值的加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val - 1, @azimuth.add(max_val - 1, 0))
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val + 1, 0))
  
  // 最大值和最小值的乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(-max_val, @azimuth.multiply(max_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))  // 特殊情况：最小值乘以-1
}

test "complex_business_logic" {
  // 测试复杂业务逻辑场景
  // 场景：计算订单总价（商品价格 × 数量 + 运费 - 折扣）
  let item_price = 50
  let quantity = 3
  let shipping = 10
  let discount = 20
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  @azimuth.assert_eq(150, subtotal)
  
  // 加上运费
  let with_shipping = @azimuth.add(subtotal, shipping)
  @azimuth.assert_eq(160, with_shipping)
  
  // 减去折扣
  let final_total = @azimuth.add(with_shipping, -discount)
  @azimuth.assert_eq(140, final_total)
  
  // 场景：计算平均分（向上取整）
  let score1 = 85
  let score2 = 92
  let score3 = 78
  let num_students = 3
  
  let total_score = @azimuth.add(@azimuth.add(score1, score2), score3)
  let average_score = @azimuth.divide_with_ceil(total_score, num_students)
  @azimuth.assert_eq(86, average_score)  // (85+92+78)/3 = 85，向上取整为86
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出后的函数仍然可用
  let _ = @azimuth.add(max_val, 1)  // 这会溢出
  let _ = @azimuth.multiply(max_val, 2)  // 这会溢出
  
  // 验证正常操作仍然工作
  @azimuth.assert_eq(8, @azimuth.add(5, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq_string("Hello, Test!", @azimuth.greet("Test"))
  
  // 测试除数为1的情况
  @azimuth.assert_eq(42, @azimuth.divide_with_ceil(42, 1))
  @azimuth.assert_eq(-42, @azimuth.divide_with_ceil(-42, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  
  // 测试被除数为0的情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -5))
}

test "mathematical_identities" {
  // 测试数学恒等式
  // 加法恒等式
  let a = 15
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(a, @azimuth.add(0, a))
  @azimuth.assert_eq(0, @azimuth.add(a, -a))
  
  // 乘法恒等式
  let b = 12
  @azimuth.assert_eq(b, @azimuth.multiply(b, 1))
  @azimuth.assert_eq(b, @azimuth.multiply(1, b))
  @azimuth.assert_eq(0, @azimuth.multiply(b, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, b))
  
  // 负数恒等式
  let c = 7
  @azimuth.assert_eq(-c, @azimuth.multiply(c, -1))
  @azimuth.assert_eq(-c, @azimuth.multiply(-1, c))
  @azimuth.assert_eq(c, @azimuth.multiply(-c, -1))
  
  // 双重否定
  let d = -9
  @azimuth.assert_eq(9, @azimuth.multiply(d, -1))
}