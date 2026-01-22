// 标准 MoonBit 测试套件
// 包含8个高质量的测试用例，覆盖核心功能

test "add_function_basic_operations" {
  // 测试基本加法运算
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(5, @azimuth.add(-10, 15))
}

test "multiply_function_properties" {
  // 测试乘法函数的数学性质
  // 交换律
  @azimuth.assert_eq(@azimuth.multiply(6, 7), @azimuth.multiply(7, 6))
  // 零元素性质
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
  // 单位元素性质
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
}

test "greet_function_internationalization" {
  // 测试问候函数的国际化支持
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 👋!", @azimuth.greet("👋"))
}

test "divide_with_ceil_practical_scenarios" {
  // 测试向上取整除法的实际应用场景
  // 包装计算：需要多少个盒子来装物品
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(23, 5))  // 23个物品，每盒5个，需要5个盒子
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(15, 6))  // 15个物品，每盒6个，需要3个盒子
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(3, 10))   // 3个物品，每盒10个，需要1个盒子
  
  // 边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))    // 0个物品
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))    // 除数为0的情况
}

test "complex_arithmetic_expressions" {
  // 测试复杂算术表达式
  // 计算：(a + b) * c - d
  let a = 8
  let b = 12
  let c = 3
  let d = 10
  
  let sum_ab = @azimuth.add(a, b)           // 8 + 12 = 20
  let product_abc = @azimuth.multiply(sum_ab, c)  // 20 * 3 = 60
  let result = @azimuth.add(product_abc, -d)      // 60 - 10 = 50
  
  @azimuth.assert_eq(50, result)
  
  // 测试嵌套运算
  let nested = @azimuth.multiply(
    @azimuth.add(5, 3), 
    @azimuth.add(7, 2)
  )  // (5+3)*(7+2) = 8*9 = 72
  @azimuth.assert_eq(72, nested)
}

test "boundary_value_analysis" {
  // 测试边界值分析
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试接近边界的值
  @azimuth.assert_eq(max_int - 1, @azimuth.add(max_int - 2, 1))
  @azimuth.assert_eq(min_int + 1, @azimuth.add(min_int + 2, -1))
  
  // 测试边界值运算
  @azimuth.assert_eq(0, @azimuth.multiply(max_int, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_int, 0))
  @azimuth.assert_eq(max_int, @azimuth.multiply(max_int, 1))
  @azimuth.assert_eq(min_int, @azimuth.multiply(min_int, 1))
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 空字符串和单字符
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 特殊字符和空白字符
  @azimuth.assert_eq_string("Hello, \t\n!", @azimuth.greet("\t\n"))
  @azimuth.assert_eq_string("Hello, space !", @azimuth.greet("space "))
  
  // 长字符串
  let long_name = "VeryLongNameThatTestsStringHandling"
  @azimuth.assert_eq_string("Hello, " + long_name + "!", @azimuth.greet(long_name))
}

test "mathematical_invariants" {
  // 测试数学不变性
  let x = 15
  let y = 7
  let z = 3
  
  // 分配律：a * (b + c) = a * b + a * c
  let left_side = @azimuth.multiply(x, @azimuth.add(y, z))
  let right_side = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  @azimuth.assert_eq(left_side, right_side)
  
  // 结合律：(a + b) + c = a + (b + c)
  let add_left = @azimuth.add(@azimuth.add(x, y), z)
  let add_right = @azimuth.add(x, @azimuth.add(y, z))
  @azimuth.assert_eq(add_left, add_right)
  
  // 乘法结合律：(a * b) * c = a * (b * c)
  let mult_left = @azimuth.multiply(@azimuth.multiply(x, y), z)
  let mult_right = @azimuth.multiply(x, @azimuth.multiply(y, z))
  @azimuth.assert_eq(mult_left, mult_right)
}