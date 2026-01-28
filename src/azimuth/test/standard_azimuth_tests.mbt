// 标准 Azimuth 测试用例
// 包含 10 个高质量的 MoonBit 测试用例，覆盖核心功能和边界情况

test "基本加法运算" {
  // 测试基本的加法运算，包括正数、负数和零
  @azimuth.assert_eq(8, @azimuth.add(5, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
}

test "基本乘法运算" {
  // 测试基本的乘法运算，包括正数、负数和零
  @azimuth.assert_eq(15, @azimuth.multiply(5, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(-15, @azimuth.multiply(-5, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(-5, -3))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

test "向上取整除法基本功能" {
  // 测试向上取整除法的基本功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0)) // 除零处理
}

test "向上取整除法负数处理" {
  // 测试向上取整除法的负数处理
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 2))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -2))
}

test "问候函数字符串处理" {
  // 测试问候函数的字符串处理能力
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "数学交换律验证" {
  // 验证加法和乘法的交换律
  let a = 17
  let b = 29
  
  // 加法交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律: a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "数学分配律验证" {
  // 验证乘法对加法的分配律
  let a = 6
  let b = 7
  let c = 8
  
  // 分配律: a * (b + c) = a * b + a * c
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "实际业务场景计算" {
  // 测试实际业务场景：计算包装需求
  let total_products = 47
  let products_per_box = 12
  
  // 计算需要的箱子数量
  let boxes_needed = @azimuth.divide_with_ceil(total_products, products_per_box)
  @azimuth.assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, products_per_box)
  @azimuth.assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = @azimuth.subtract(total_capacity, total_products)
  @azimuth.assert_eq(1, remaining_space)
}

test "复杂计算流水线" {
  // 测试复杂计算流水线
  let base_value = 15
  
  // 计算流程：((base * 2) + 10) / 5
  let step1 = @azimuth.multiply(base_value, 2)      // 30
  let step2 = @azimuth.add(step1, 10)               // 40
  let result = @azimuth.divide_with_ceil(step2, 5)  // 8
  
  @azimuth.assert_eq(8, result)
}

test "边界条件和错误处理" {
  // 测试边界条件和错误处理
  // 极值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -1))
  
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 零乘法测试
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
  @azimuth.assert_eq(0, @azimuth.multiply(100, 0))
}