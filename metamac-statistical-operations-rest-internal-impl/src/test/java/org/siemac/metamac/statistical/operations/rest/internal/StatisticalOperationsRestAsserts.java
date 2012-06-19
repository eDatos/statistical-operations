package org.siemac.metamac.statistical.operations.rest.internal;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.List;

import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.LocalisedString;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;

public class StatisticalOperationsRestAsserts {

    // TODO titleS
    public static void assertEqualsOperation(Operation expected, Operation actual) {
        assertEquals(expected.getCode(), expected.getCode());
        assertEqualsInternationalString(expected.getTitles(), actual.getTitles());
    }
    
    public static void assertEqualsInternationalString(List<LocalisedString> expecteds, List<LocalisedString> actuals) {
        if (actuals == null && expecteds == null) {
            return;
        } else if ((actuals != null && expecteds == null) || (actuals == null && expecteds != null)) {
            fail();
        }
        assertEquals(expecteds.size(), actuals.size());
        
        for (int i = 0; i < expecteds.size(); i++) {
            LocalisedString expected = expecteds.get(i);
            LocalisedString actual = actuals.get(i);
            assertEquals(expected.getLabel(), actual.getLabel());
            assertEquals(expected.getLocale(), actual.getLocale());
        }
    }
}
