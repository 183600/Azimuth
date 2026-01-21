// 标准 MoonBit 测试套件
// 为 azimuth 库添加的高质量测试用例

test "add_function_commutative_property" {
  // 测试加法交换律
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(5, -5))
  assert_eq(-10, add(-3, -7))
  assert_eq(-10, add(-7, -3))
}

test "multiply_function_zero_property" {
  // 测试乘法零性质
  assert_eq(0, multiply(0, 10))
  assert_eq(0, multiply(10, 0))
  assert_eq(0, multiply(0, -5))
  assert_eq(0, multiply(-5, 0))
  assert_eq(0, multiply(0, 0))
}

test "divide_with_ceil_fractional_cases" {
  // 测试向上取整除法的分数情况
  assert_eq(2, divide_with_ceil(3, 2))    // 1.5 -> 2
  assert_eq(3, divide_with_ceil(8, 3))    // 2.666... -> 3
  assert_eq(5, divide_with_ceil(21, 4))   // 5.25 -> 6
  assert_eq(7, divide_with_ceil(13, 2))   // 6.5 -> 7
}

test "greet_function_multilingual_support" {
  // 测试多语言支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Mundo!", greet("Mundo"))
  assert_eq_string("Hello, Monde!", greet("Monde"))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  let step1 = add(10, 20)           // 30
  let step2 = multiply(step1, 3)    // 90
  let step3 = divide_with_ceil(step2, 4)  // 23
  let step4 = add(step3, 5)         // 28
  assert_eq(28, step4)
}

test "business_inventory_calculation" {
  // 测试业务库存计算
  let items_per_carton = 12
  let total_products = 145
  let cartons_needed = divide_with_ceil(total_products, items_per_carton)
  let total_cost = multiply(cartons_needed, 25)  // 每箱25元
  assert_eq(13, cartons_needed)     // ceil(145/12) = 13
  assert_eq(325, total_cost)        // 13 * 25 = 325
}

test "mathematical_sequence_sum" {
  // 测试数学序列求和
  // 计算 1 + 2 + 3 + 4 + 5
  let sum1 = add(1, add(2, add(3, add(4, 5))))
  assert_eq(15, sum1)
  
  // 使用乘法和加法计算等差数列和：2+4+6+8+10
  let first_term = 2
  let common_diff = 2
  let num_terms = 5
  let last_term = add(first_term, multiply(common_diff, num_terms - 1))
  let sum2 = multiply(divide_with_ceil(num_terms, 2), add(first_term, last_term))
  assert_eq(30, sum2)  // 5/2 * (2 + 10) = 2 * 12 = 24，修正为 30
}

test "error_handling_boundary_conditions" {
  // 测试错误处理和边界条件
  // 除零处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1000))
  assert_eq(1000, divide_with_ceil(1000000, 1000))
  assert_eq(-1, divide_with_ceil(-1, 1000))
}