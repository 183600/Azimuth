// 标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法编写的基础测试套件

test "basic_addition_functionality" {
  // 基础加法功能测试
  azimuth::assert_eq(0, azimuth::add(0, 0))
  azimuth::assert_eq(5, azimuth::add(2, 3))
  azimuth::assert_eq(-1, azimuth::add(-3, 2))
  azimuth::assert_eq(100, azimuth::add(50, 50))
  azimuth::assert_eq(-100, azimuth::add(-50, -50))
}

test "basic_multiplication_functionality" {
  // 基础乘法功能测试
  azimuth::assert_eq(0, azimuth::multiply(5, 0))
  azimuth::assert_eq(6, azimuth::multiply(2, 3))
  azimuth::assert_eq(-15, azimuth::multiply(-3, 5))
  azimuth::assert_eq(15, azimuth::multiply(-3, -5))
  azimuth::assert_eq(10000, azimuth::multiply(100, 100))
}

test "basic_subtraction_functionality" {
  // 基础减法功能测试
  azimuth::assert_eq(5, azimuth::subtract(10, 5))
  azimuth::assert_eq(-5, azimuth::subtract(5, 10))
  azimuth::assert_eq(0, azimuth::subtract(15, 15))
  azimuth::assert_eq(20, azimuth::subtract(10, -10))
  azimuth::assert_eq(-20, azimuth::subtract(-10, 10))
}

test "divide_with_ceil_basic_cases" {
  // 向上取整除法基础情况测试
  azimuth::assert_eq(4, azimuth::divide_with_ceil(10, 3))
  azimuth::assert_eq(3, azimuth::divide_with_ceil(9, 3))
  azimuth::assert_eq(1, azimuth::divide_with_ceil(1, 5))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 10))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(10, 0))
}

test "divide_with_ceil_negative_cases" {
  // 向上取整除法负数情况测试
  azimuth::assert_eq(-3, azimuth::divide_with_ceil(-10, 3))
  azimuth::assert_eq(-4, azimuth::divide_with_ceil(10, -3))
  azimuth::assert_eq(4, azimuth::divide_with_ceil(-10, -3))
  azimuth::assert_eq(-1, azimuth::divide_with_ceil(-1, 2))
  azimuth::assert_eq(1, azimuth::divide_with_ceil(-1, -2))
}

test "greet_function_standard_cases" {
  // 问候函数标准情况测试
  azimuth::assert_eq_string("Hello, World!", azimuth::greet("World"))
  azimuth::assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
  azimuth::assert_eq_string("Hello, 123!", azimuth::greet("123"))
  azimuth::assert_eq_string("Hello, Test!", azimuth::greet("Test"))
}

test "mathematical_commutative_properties" {
  // 数学交换律性质测试
  let a = 12
  let b = 8
  
  // 加法交换律: a + b = b + a
  azimuth::assert_eq(azimuth::add(a, b), azimuth::add(b, a))
  
  // 乘法交换律: a * b = b * a
  azimuth::assert_eq(azimuth::multiply(a, b), azimuth::multiply(b, a))
}

test "mathematical_identity_properties" {
  // 数学单位元性质测试
  let x = 42
  
  // 加法单位元: x + 0 = x
  azimuth::assert_eq(azimuth::add(x, 0), x)
  
  // 乘法单位元: x * 1 = x
  azimuth::assert_eq(azimuth::multiply(x, 1), x)
  
  // 减法单位元: x - 0 = x
  azimuth::assert_eq(azimuth::subtract(x, 0), x)
}

test "boundary_value_zero_handling" {
  // 零值边界处理测试
  // 测试各种涉及零的运算
  azimuth::assert_eq(0, azimuth::add(0, 0))
  azimuth::assert_eq(0, azimuth::multiply(0, 100))
  azimuth::assert_eq(0, azimuth::subtract(0, 0))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 10))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 0))
  
  // 测试零与其他数的运算
  azimuth::assert_eq(100, azimuth::add(0, 100))
  azimuth::assert_eq(0, azimuth::multiply(100, 0))
  azimuth::assert_eq(100, azimuth::subtract(100, 0))
  azimuth::assert_eq(-100, azimuth::subtract(0, 100))
}

test "complex_calculation_chain" {
  // 复杂计算链测试
  // 测试多个函数组合使用的场景
  
  let initial = 10
  
  // 计算链: ((initial * 2) + 5) - 3) / 4
  let step1 = azimuth::multiply(initial, 2)        // 10 * 2 = 20
  let step2 = azimuth::add(step1, 5)               // 20 + 5 = 25
  let step3 = azimuth::subtract(step2, 3)          // 25 - 3 = 22
  let step4 = azimuth::divide_with_ceil(step3, 4)  // ceil(22/4) = 6
  
  // 验证每一步的结果
  azimuth::assert_eq(step1, 20)
  azimuth::assert_eq(step2, 25)
  azimuth::assert_eq(step3, 22)
  azimuth::assert_eq(step4, 6)
}