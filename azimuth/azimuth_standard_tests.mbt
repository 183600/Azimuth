// Azimuth 项目标准 MoonBit 测试用例
// 包含 10 个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(8, add(3, 5))
  assert_eq(15, multiply(3, 5))
  assert_eq(-2, subtract(3, 5))
  assert_eq(2, divide_with_ceil(7, 4))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "mathematical_commutative_properties" {
  // 数学交换律性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律: a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律: a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(20, add(12, 8))
  assert_eq(96, multiply(12, 8))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-8, add(-3, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(-1, divide_with_ceil(-3, 4))
  assert_eq(2, divide_with_ceil(-7, -4))
  assert_eq(-2, subtract(-5, -3))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "boundary_values_and_limits" {
  // 边界值和极限测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(0, subtract(5, 5))
}

test "complex_business_calculation" {
  // 复杂业务计算测试：库存管理场景
  let total_products = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  // 验证需要的箱子数量
  assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(11, remaining_space)
}

test "mathematical_distributive_property" {
  // 数学分配律测试
  let a = 6
  let b = 4
  let c = 3
  
  // 分配律: a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  
  // 验证具体数值: 6 * (4 + 3) = 6 * 4 + 6 * 3 = 42
  assert_eq(42, left_side)
  assert_eq(42, right_side)
}

test "large_number_operations" {
  // 大数运算测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(1001, divide_with_ceil(1000000, 999))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
  assert_eq(0, subtract(1000000, 1000000))
}

test "sequential_calculation_workflow" {
  // 连续计算工作流测试
  let initial_value = 10
  
  // 第一步：加法
  let step1 = add(initial_value, 8)  // 18
  
  // 第二步：乘法
  let step2 = multiply(step1, 3)     // 54
  
  // 第三步：向上取整除法
  let step3 = divide_with_ceil(step2, 20)  // 3
  
  // 第四步：减法
  let final_result = subtract(step3, 1)    // 2
  
  assert_eq(18, step1)
  assert_eq(54, step2)
  assert_eq(3, step3)
  assert_eq(2, final_result)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边缘情况测试
  // 除零错误的全面测试
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(9999, 0))
  assert_eq(0, divide_with_ceil(-9999, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 特殊字符串输入测试
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, special@chars!", greet("special@chars"))
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
  
  // 数学恒等式验证
  let x = 42
  assert_eq(x, add(x, 0))      // 加法恒等式
  assert_eq(x, multiply(x, 1)) // 乘法恒等式
  assert_eq(0, multiply(x, 0)) // 零乘法性质
  assert_eq(0, subtract(x, x)) // 自减性质
}