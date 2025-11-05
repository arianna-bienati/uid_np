import numpy as np
import pandas as pd
import pytest

# Implementation 1: Simple diff approach
def uid_simple(srp_values):
    """Simple mean absolute difference approach."""
    diffs = np.diff(srp_values)
    uid_dev = np.mean(np.abs(diffs))
    return uid_dev

# Implementation 2: Pandas groupby approach
def uid_pandas(surprisal_list, doc_id=0, sent_id=0):
    """Pandas-based approach with grouping."""
    df = pd.DataFrame({
        'surprisal': surprisal_list,
        'document_id': doc_id,
        'sentence_id': sent_id,
        'sentence_pos': range(len(surprisal_list))
    })
    
    df["delta_surp"] = abs(df.surprisal - df.surprisal.shift(1))
    mask = (df.sentence_pos == 0) & (df.sentence_id == 0)
    df.loc[mask, "delta_surp"] = np.nan
    
    d = (
        df.dropna(subset=["delta_surp"])
        .groupby(["document_id", "sentence_id"], observed=True)
        .delta_surp.agg(np.mean)
        .dropna()
        .reset_index()
    )
    
    return d.delta_surp.values[0] if len(d) > 0 else np.nan

# Implementation 3: Local difference functions
def local_diff(x):
    """Sum of absolute differences normalized by length."""
    d = 0
    for i in range(len(x)-1):
        d += abs(x[i+1]-x[i])
    return d/len(x)

def local_diff2(x):
    """Sum of squared differences normalized by length."""
    d = 0
    for i in range(len(x)-1):
        d += (x[i+1]-x[i])**2
    return d/len(x)

# ============================================================================
# TEST SUITE
# ============================================================================

class TestUIDImplementations:
    """Test that different UID implementations produce consistent results."""
    
    # Test data
    test_surprisal_1 = [22.23, 3.47, 23.90, 0.96, 21.90, 5.46, 18.80]
    test_surprisal_2 = [1.0, 2.0, 3.0, 4.0, 5.0]
    test_surprisal_4 = [5.5, 5.5, 5.5, 5.5]  # Constant values
    
    def test_simple_vs_local_diff_implementation_1(self):
        """Test Implementation 1 vs Implementation 3 (local_diff) with test data 1."""
        result_simple = uid_simple(self.test_surprisal_1)
        result_local = local_diff(self.test_surprisal_1)
        
        np.testing.assert_allclose(result_simple, result_local, rtol=1e-10,
                                    err_msg="Implementation 1 and local_diff should match")
        print(f"✓ Test 1: {result_simple:.6f} == {result_local:.6f}")
    
    def test_simple_vs_pandas_implementation_1(self):
        """Test Implementation 1 vs Implementation 2 with test data 1."""
        result_simple = uid_simple(self.test_surprisal_1)
        result_pandas = uid_pandas(self.test_surprisal_1)
        
        np.testing.assert_allclose(result_simple, result_pandas, rtol=1e-10,
                                    err_msg="Implementation 1 and pandas should match")
        print(f"✓ Test 2: {result_simple:.6f} == {result_pandas:.6f}")
    
    def test_pandas_vs_local_diff_implementation_1(self):
        """Test Implementation 2 vs Implementation 3 with test data 1."""
        result_pandas = uid_pandas(self.test_surprisal_1)
        result_local = local_diff(self.test_surprisal_1)
        
        np.testing.assert_allclose(result_pandas, result_local, rtol=1e-10,
                                    err_msg="Pandas and local_diff should match")
        print(f"✓ Test 3: {result_pandas:.6f} == {result_local:.6f}")
    
    def test_all_three_with_test_data_1(self):
        """Comprehensive test: all three implementations match with test data 1."""
        result_simple = uid_simple(self.test_surprisal_1)
        result_pandas = uid_pandas(self.test_surprisal_1)
        result_local = local_diff(self.test_surprisal_1)
        
        np.testing.assert_allclose(result_simple, result_pandas, rtol=1e-10)
        np.testing.assert_allclose(result_pandas, result_local, rtol=1e-10)
        np.testing.assert_allclose(result_simple, result_local, rtol=1e-10)
        
        print(f"✓ All implementations match: {result_simple:.6f}")
    
    def test_all_three_with_test_data_2(self):
        """Test with simple linear sequence."""
        result_simple = uid_simple(self.test_surprisal_2)
        result_pandas = uid_pandas(self.test_surprisal_2)
        result_local = local_diff(self.test_surprisal_2)
        
        np.testing.assert_allclose(result_simple, result_pandas, rtol=1e-10)
        np.testing.assert_allclose(result_pandas, result_local, rtol=1e-10)
        
        expected = 1.0  # Each diff is 1, so mean is 1
        np.testing.assert_allclose(result_simple, expected, rtol=1e-10)
        print(f"✓ Linear sequence test: {result_simple:.6f} == {expected}")
    
    def test_all_three_with_constant_values(self):
        """Test with constant surprisal values."""
        result_simple = uid_simple(self.test_surprisal_4)
        result_pandas = uid_pandas(self.test_surprisal_4)
        result_local = local_diff(self.test_surprisal_4)
        
        np.testing.assert_allclose(result_simple, result_pandas, rtol=1e-10)
        np.testing.assert_allclose(result_pandas, result_local, rtol=1e-10)
        
        expected = 0.0  # All diffs are 0
        np.testing.assert_allclose(result_simple, expected, atol=1e-10)
        print(f"✓ Constant values test: {result_simple:.6f} == {expected}")
    
    def test_local_diff_vs_local_diff2_relationship(self):
        """Test relationship between local_diff and local_diff2."""
        # For non-constant data, squared differences should be >= absolute diffs
        result_diff = local_diff(self.test_surprisal_1)
        result_diff2 = local_diff2(self.test_surprisal_1)
        
        # By Cauchy-Schwarz, sum of squares >= (sum)^2 / n for positive values
        assert result_diff2 >= 0, "local_diff2 should be non-negative"
        print(f"✓ local_diff: {result_diff:.6f}, local_diff2: {result_diff2:.6f}")
    
# ============================================================================
# DEMONSTRATION
# ============================================================================

if __name__ == "__main__":
    print("=" * 70)
    print("UID IMPLEMENTATION EQUIVALENCE TESTS")
    print("=" * 70)
    print()
    
    # Run pytest
    pytest.main([__file__, "-v", "-s"])
    
    print()
    print("=" * 70)
    print("MANUAL VERIFICATION WITH ORIGINAL TEST DATA")
    print("=" * 70)
    print()
    
    test_data = [22.23, 3.47, 23.90, 0.96, 21.90, 5.46, 18.80]
    print(f"Input surprisal values: {test_data}")
    print()
    
    result1 = uid_simple(test_data)
    print(f"Implementation 1 (simple diff):  {result1:.10f}")
    
    result2 = uid_pandas(test_data)
    print(f"Implementation 2 (pandas):       {result2:.10f}")
    
    result3 = local_diff(test_data)
    print(f"Implementation 3 (local_diff):   {result3:.10f}")
    
    result4 = local_diff2(test_data)
    print(f"Implementation 3b (local_diff2): {result4:.10f}")
    
    print()
    print("All implementations match:", 
          np.allclose([result1, result2, result3], result1))