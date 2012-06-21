package org.siemac.metamac.statistical.operations.rest.internal;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.InputStream;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.apache.cxf.helpers.IOUtils;
import org.apache.cxf.io.CachedOutputStream;
import org.siemac.metamac.rest.common.v1_0.domain.InternationalString;
import org.siemac.metamac.rest.common.v1_0.domain.Link;
import org.siemac.metamac.rest.common.v1_0.domain.LocalisedString;
import org.siemac.metamac.rest.common.v1_0.domain.Resource;

// TODO Pasar a librería común
public class RestAsserts {

    public static void assertEqualsResources(List<Resource> expecteds, List<Resource> actuals) {
        assertEqualsNullability(expecteds, actuals);
        if (expecteds == null) {
            return;
        }
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
        assertEqualsNullability(expected, actual);
        if (expected == null) {
            return;
        }
        assertEquals(expected.getId(), actual.getId());
        assertEquals(expected.getKind(), actual.getKind());
        assertEqualsLink(expected.getLink(), actual.getLink());
        assertEqualsInternationalString(expected.getTitle(), actual.getTitle());
    }
    
    public static void assertEqualsLink(Link expected, Link actual) {
        assertEqualsNullability(expected, actual);
        if (expected == null) {
            return;
        }
        assertEquals(expected.getRel(), actual.getRel());
        assertEquals(expected.getHref(), actual.getHref());
    }

    public static void assertEqualsInternationalString(InternationalString expecteds, InternationalString actuals) {
        assertEqualsNullability(expecteds, actuals);
        if (expecteds == null) {
            return;
        }
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
    
    public static void assertEqualsResponse(InputStream responseExpected, InputStream responseActual) throws Exception {
        String actual = getStringFromInputStream(responseActual);
        String expected = getStringFromInputStream(responseExpected);
        // Only compare lenghts of response, because items in response can be received in different order (example: LocalisedString)
        String actualFormatted = StringUtils.deleteWhitespace(actual);
        String expectedFormatted = StringUtils.deleteWhitespace(expected);
        assertEquals(actualFormatted.length(), expectedFormatted.length());
    }
    
    private static String getStringFromInputStream(InputStream in) throws Exception {
        CachedOutputStream bos = new CachedOutputStream();
        IOUtils.copy(in, bos);
        in.close();
        bos.close();
        return bos.getOut().toString();
    }
    
    private static void assertEqualsNullability(Object expected, Object actual) {
        if (actual == null && expected == null) {
            return;
        } else if ((actual != null && expected == null) || (actual == null && expected != null)) {
            fail();
        }
    }
}
