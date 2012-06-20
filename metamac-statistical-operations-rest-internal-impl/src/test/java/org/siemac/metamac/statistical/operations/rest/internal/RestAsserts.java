package org.siemac.metamac.statistical.operations.rest.internal;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.List;

import org.siemac.metamac.rest.v1_0.domain.InternationalString;
import org.siemac.metamac.rest.v1_0.domain.Link;
import org.siemac.metamac.rest.v1_0.domain.LocalisedString;
import org.siemac.metamac.rest.v1_0.domain.Resource;

// TODO Pasar a librería común
public class RestAsserts {

    public static void assertEqualsResources(List<Resource> expecteds, List<Resource> actuals) {
        assertEqualsNullability(expecteds, actuals);
        assertEquals(expecteds.size(), actuals.size());
        for (Resource expected : expecteds) {
            boolean existsItem = false;
            for (Resource actual : actuals) {
                if (expected.getId().equals(actual.getId())) {
                    assertEqualsResource(expected, actual);
                    existsItem = true;
                }
            }
            assertTrue(existsItem);
        }
    }

    public static void assertEqualsResource(Resource expected, Resource actual) {
        assertEquals(expected.getId(), actual.getId());
        assertEquals(expected.getKind(), actual.getKind());
        assertEqualsLink(expected.getLink(), actual.getLink());
        assertEqualsInternationalString(expected.getTitle(), actual.getTitle());
    }
    
    public static void assertEqualsLink(Link expected, Link actual) {
        assertEquals(expected.getRel(), actual.getRel());
        assertEquals(expected.getHref(), actual.getHref());
    }

    public static void assertEqualsInternationalString(InternationalString expecteds, InternationalString actuals) {
        assertEqualsNullability(expecteds, actuals);
        assertEquals(expecteds.getTexts().size(), actuals.getTexts().size());
        for (LocalisedString expected : expecteds.getTexts()) {
            boolean existsItem = false;
            for (LocalisedString actual : actuals.getTexts()) {
                if (expected.getLocale().equals(actual.getLocale())) {
                    assertEquals(expected.getLabel(), actual.getLabel());
                    existsItem = true;
                }
            }
            assertTrue(existsItem);
        }
    }
    
    private static void assertEqualsNullability(Object expected, Object actual) {
        if (actual == null && expected == null) {
            return;
        } else if ((actual != null && expected == null) || (actual == null && expected != null)) {
            fail();
        }
    }
}
