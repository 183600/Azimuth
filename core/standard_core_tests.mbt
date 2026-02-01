// 标准 MoonBit 核心库测试用例
// 包含10个高质量的测试用例，使用标准 MoonBit 测试语法

// 测试 int 模块的基本功能
test "int_module_basic_functions" {
  // 测试 int 模块的最大值和最小值
  inspect(@int.max_value, content="2147483647")
  inspect(@int.min_value, content="-2147483648")
  
  // 测试基本的整数运算
  inspect(Int::abs(5), content="5")
  inspect(Int::abs(-5), content="5")
  inspect(Int::abs(0), content="0")
}

// 测试 string 模块的基本功能
test "string_module_basic_functions" {
  // 测试字符串默认值
  inspect(String::default(), content="")
  inspect(@string.default(), content="")
  
  // 测试基本字符串操作
  let empty_string = ""
  let test_string = "Hello, MoonBit!"
  
  inspect(empty_string.length(), content="0")
  inspect(test_string.length(), content="14")
}

// 测试 math 模块的代数函数
test "math_algebraic_functions" {
  // 测试立方根函数
  inspect(@math.cbrtf(1.0), content="1")
  inspect(@math.cbrtf(27.0), content="3")
  inspect(@math.cbrtf(125.0), content="5")
  inspect(@math.cbrtf(0.0), content="0")
  inspect(@math.cbrtf(-1.0), content="-1")
  
  // 测试斜边函数
  inspect(@math.hypotf(3.0, 4.0), content="5")
  inspect(@math.hypotf(5.0, 12.0), content="13")
  inspect(@math.hypotf(0.0, 0.0), content="0")
}

// 测试数值类型转换
test "numeric_type_conversions" {
  // 测试整数与字节转换
  let test_int = 123456789
  let test_uint = test_int.reinterpret_as_uint()
  
  // 测试字节转换函数
  let big_endian_bytes = @int.Int::to_be_bytes(test_int)
  let little_endian_bytes = @int.Int::to_le_bytes(test_int)
  
  inspect(big_endian_bytes.length(), content="4")
  inspect(little_endian_bytes.length(), content="4")
  
  // 验证字节顺序不同
  assert_true(big_endian_bytes[0] != little_endian_bytes[0])
  assert_true(big_endian_bytes[3] != little_endian_bytes[3])
}

// 测试边界值处理
test "boundary_value_handling" {
  // 测试整数边界值
  let max_int = @int.max_value
  let min_int = @int.min_value
  
  inspect(Int::abs(max_int), content="2147483647")
  inspect(Int::abs(min_int), content="-2147483648")
  
  // 测试零值的特殊情况
  inspect(@math.cbrtf(0.0), content="0")
  inspect(@math.hypotf(0.0, 5.0), content="5")
  inspect(@math.hypotf(5.0, 0.0), content="5")
}

// 测试数学恒等式
test "mathematical_identities" {
  // 验证 cbrt(x^3) = x (对于正数)
  inspect(@math.cbrtf(@math.powf(2.0, 3.0)), content="2")
  inspect(@math.cbrtf(@math.powf(3.0, 3.0)), content="3")
  inspect(@math.cbrtf(@math.powf(5.0, 3.0)), content="5")
  
  // 测试斜边函数的对称性
  let hypot1 = @math.hypotf(3.0, 4.0)
  let hypot2 = @math.hypotf(4.0, 3.0)
  assert_true(hypot1.is_close(hypot2))
  
  let hypot3 = @math.hypotf(5.0, 12.0)
  let hypot4 = @math.hypotf(12.0, 5.0)
  assert_true(hypot3.is_close(hypot4))
}

// 测试错误处理和特殊情况
test "error_handling_and_special_cases" {
  // 测试无穷大和NaN的处理
  let inf = @float.infinity
  let neg_inf = @float.neg_infinity
  
  // hypotf 对无穷大的处理
  inspect(@math.hypotf(inf, 3.0), content="Infinity")
  inspect(@math.hypotf(3.0, inf), content="Infinity")
  inspect(@math.hypotf(neg_inf, 3.0), content="Infinity")
  
  // cbrtf 对特殊值的处理
  let cbrt_inf = @math.cbrtf(inf)
  let cbrt_neg_inf = @math.cbrtf(neg_inf)
  
  assert_true(cbrt_inf.is_inf())
  assert_true(cbrt_neg_inf.is_neg_inf())
}

// 测试性能相关操作
test "performance_related_operations" {
  // 测试大数运算
  let num1 = 1000000
  let num2 = 2000000
  let num3 = 3000000
  
  let abs_result1 = Int::abs(num1)
  let abs_negative1 = Int::abs(-num1)
  
  inspect(abs_result1, content="1000000")
  inspect(abs_negative1, content="1000000")
  
  let abs_result2 = Int::abs(num2)
  let abs_negative2 = Int::abs(-num2)
  
  inspect(abs_result2, content="2000000")
  inspect(abs_negative2, content="2000000")
  
  // 测试浮点数运算的精度
  let precise_calculation = @math.hypotf(0.3, 0.4)
  assert_true(precise_calculation > 0.49 && precise_calculation < 0.51)
}

// 测试综合应用场景
test "comprehensive_application_scenarios" {
  // 场景：计算3D空间中点到原点的距离
  let x = 3.0
  let y = 4.0
  let z = 12.0
  
  // 先计算xy平面的距离，再结合z轴
  let xy_distance = @math.hypotf(x, y)
  let xyz_distance = @math.hypotf(xy_distance, z)
  
  // 验证结果 (应该是13.0，因为3-4-5-12-13是扩展的勾股数)
  inspect(xyz_distance, content="13")
  
  // 场景：计算立方体的体积和边长关系
  let volume = 27.0
  let edge_length = @math.cbrtf(volume)
  inspect(edge_length, content="3")
}

// 测试内存和字节操作
test "memory_and_byte_operations" {
  // 测试不同整数的字节表示
  let value1 = 0
  let value2 = 1
  let value3 = -1
  
  let big_endian1 = @int.Int::to_be_bytes(value1)
  let little_endian1 = @int.Int::to_le_bytes(value1)
  
  // 验证字节长度
  inspect(big_endian1.length(), content="4")
  inspect(little_endian1.length(), content="4")
  
  // 验证字节顺序相反
  assert_true(big_endian1[0] == little_endian1[3])
  assert_true(big_endian1[1] == little_endian1[2])
  assert_true(big_endian1[2] == little_endian1[1])
  assert_true(big_endian1[3] == little_endian1[0])
  
  // 测试特殊值的字节表示
  let max_val_bytes = @int.Int::to_be_bytes(@int.max_value)
  let min_val_bytes = @int.Int::to_be_bytes(@int.min_value)
  
  // 验证最大值和最小值的字节表示不同
  assert_true(max_val_bytes[0] != min_val_bytes[0])
}

// 测试数学函数的边界情况
test "math_function_edge_cases" {
  // 测试极值的立方根
  inspect(@math.cbrtf(@float.infinity), content="Infinity")
  inspect(@math.cbrtf(@float.neg_infinity), content="-Infinity")
  inspect(@math.cbrtf(@float.not_a_number), content="NaN")
  
  // 测试极值的斜边计算
  inspect(@math.hypotf(@float.infinity, @float.infinity), content="Infinity")
  inspect(@math.hypotf(@float.not_a_number, 1.0), content="NaN")
  
  // 测试非常小的数值
  let tiny = 1.0e-10
  let small_hypot = @math.hypotf(tiny, tiny)
  assert_true(small_hypot > 0.0)
  
  // 测试数学常数的运算
  let pi_square = @math.hypotf(@math.pi, 0.0)
  assert_true(pi_square.is_close(@math.pi))
}