// 简单的测试验证脚本
// 导入azimuth库
use azimuth

test "validate_import" {
  // 简单验证导入是否工作
  assert_eq(5, azimuth.add(2, 3))
}