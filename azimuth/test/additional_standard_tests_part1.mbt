// 额外的标准 MoonBit 测试用例 - 第1部分
// 测试 azimuth 包中的核心函数的各种场景

test "mathematical_properties_commutative" {
  // 测试数学交换律性质
  let a = 15
  let b = 27
  
  // 加法交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律: a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "mathematical_properties_associative" {
  // 测试数学结合律性质
  let a = 10
  let b = 20
  let c = 30
  
  // 加法结合律: (a + b) + c = a + (b + c)
  let left_add = @azimuth.add(@azimuth.add(a, b), c)
  let right_add = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left_add, right_add)
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  let left_mul = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let right_mul = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(left_mul, right_mul)
}

test "mathematical_properties_distributive" {
  // 测试数学分配律性质: a * (b + c) = a * b + a * c
  let a = 5
  let b = 12
  let c = 8
  
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
}