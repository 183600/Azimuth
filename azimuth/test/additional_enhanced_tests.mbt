// 额外的增强测试用例 - 补充现有测试覆盖
// 使用标准的 MoonBit 测试语法，包含8个新的测试用例

test "add_function_commutative_property" {
  // 测试加法交换律
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(15, @azimuth.add(8, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-10, @azimuth.add(-7, -3))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律
  let a = 6
  let b = 4
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
  
  // (a + b) * c = a * c + b * c
  let left_side2 = @azimuth.multiply(@azimuth.add(a, b), c)
  let right_side2 = @azimuth.add(@azimuth.multiply(a, c), @azimuth.multiply(b, c))
  @azimuth.assert_eq(left_side2, right_side2)
}

test "divide_with_ceil_precision_tests" {
  // 测试向上取整除法的精度
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 2))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(5, 2))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(7, 2))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(9, 2))
  
  // 测试负数精度
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-1, 2))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-3, 2))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 2))
}

test "greet_function_special_characters" {
  // 测试问候函数的特殊字符处理
  @azimuth.assert_eq_string("Hello, @user!", @azimuth.greet("@user"))
  @azimuth.assert_eq_string("Hello, #123!", @azimuth.greet("#123"))
  @azimuth.assert_eq_string("Hello, $value!", @azimuth.greet("$value"))
  @azimuth.assert_eq_string("Hello, %percent!", @azimuth.greet("%percent"))
  @azimuth.assert_eq_string("Hello, &amp!", @azimuth.greet("&amp"))
}

test "arithmetic_identity_elements" {
  // 测试算术恒等元素
  // 加法恒等元素：0
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
  @azimuth.assert_eq(-17, @azimuth.add(0, -17))
  
  // 乘法恒等元素：1
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
  @azimuth.assert_eq(-17, @azimuth.multiply(-17, 1))
  @azimuth.assert_eq(-17, @azimuth.multiply(1, -17))
  
  // 乘法零元素：0
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
  @azimuth.assert_eq(0, @azimuth.multiply(-17, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -17))
}

test "divide_with_ceil_real_world_scenarios" {
  // 测试实际应用场景
  // 计算需要的容器数量
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(25, 10))  // 25个物品，每箱10个，需要3箱
  @azimuth.assert_eq(6, @azimuth.divide_with_ceil(41, 8))   // 41个物品，每箱8个，需要6箱
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 10))   // 5个物品，每箱10个，需要1箱
  
  // 计算页数
  @azimuth.assert_eq(10, @azimuth.divide_with_ceil(95, 10))  // 95行，每页10行，需要10页
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(20, 8))    // 20行，每页8行，需要3页
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  let initial_value = 10
  
  // 步骤1：加法
  let step1 = @azimuth.add(initial_value, 5)
  @azimuth.assert_eq(15, step1)
  
  // 步骤2：乘法
  let step2 = @azimuth.multiply(step1, 2)
  @azimuth.assert_eq(30, step2)
  
  // 步骤3：再次加法
  let step3 = @azimuth.add(step2, -10)
  @azimuth.assert_eq(20, step3)
  
  // 步骤4：除法向上取整
  let final_result = @azimuth.divide_with_ceil(step3, 6)
  @azimuth.assert_eq(4, final_result)
}

test "greet_function_multilingual_support" {
  // 测试多语言支持
  @azimuth.assert_eq_string("Hello, Español!", @azimuth.greet("Español"))
  @azimuth.assert_eq_string("Hello, Français!", @azimuth.greet("Français"))
  @azimuth.assert_eq_string("Hello, Deutsch!", @azimuth.greet("Deutsch"))
  @azimuth.assert_eq_string("Hello, Português!", @azimuth.greet("Português"))
  @azimuth.assert_eq_string("Hello, 日本語!", @azimuth.greet("日本語"))
  @azimuth.assert_eq_string("Hello, 한국어!", @azimuth.greet("한국어"))
  @azimuth.assert_eq_string("Hello, العربية!", @azimuth.greet("العربية"))
  @azimuth.assert_eq_string("Hello, עברית!", @azimuth.greet("עברית"))
}