package org.siemac.metamac.statistical.operations.rest.internal;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.List;

import org.siemac.metamac.rest.v1_0.domain.InternationalString;
import org.siemac.metamac.rest.v1_0.domain.Link;
import org.siemac.metamac.rest.v1_0.domain.LocalisedString;
import org.siemac.metamac.rest.v1_0.domain.Resource;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;

public class StatisticalOperationsRestAsserts {

    public static void assertEqualsOperation(Operation expected, Operation actual) {
        assertEquals(expected.getId(), expected.getId());
        assertEquals(expected.getLink().getRel(), expected.getLink().getRel());
        assertEquals(expected.getLink().getHref(), expected.getLink().getHref());
        assertEqualsInternationalString(expected.getTitle(), actual.getTitle());
        assertEqualsInternationalString(expected.getAcronym(), actual.getAcronym());
        assertEqualsResources(expected.getFamilies(), actual.getFamilies());
        // TODO SUBJECT_AREA
        // TODO SUBJECT_CODE
        // TODO SECUNDARY_SUBJECT_AREAS
        // TODO SECUNDARY_SUBJECT_CODES
        assertEqualsInternationalString(expected.getObjective(), actual.getObjective());
        assertEqualsInternationalString(expected.getDescription(), actual.getDescription());
        // TODO INSTANCE_CODE
        // TODO INSTANCE_TITLE
        // TODO survey type: lista? mirar excel
        assertEquals(expected.getSurveyType(), actual.getSurveyType());
        // TODO officialy type: lista? mirar excel
        assertEquals(expected.getOfficialityType(), actual.getOfficialityType());
        assertEquals(expected.isIndicatorSystem(), actual.isIndicatorSystem());
        // TODO PRODUCER
        // TODO REGIONAL_RESPONSIBLE
        // TODO REGIONAL_CONTRIBUTOR
        assertEquals(expected.getInternalInventoryDate(), actual.getInternalInventoryDate());
        assertEquals(expected.isCurrentlyActive(), actual.isCurrentlyActive());
        assertEquals(expected.getStatus(), actual.getStatus());
        assertEquals(expected.getProcStatus(), actual.getProcStatus());
        // TODO PUBLISHER
        assertEqualsInternationalString(expected.getRelPolUsAc(), actual.getRelPolUsAc());
        assertEquals(expected.getRelPolUsAcUrl(), actual.getRelPolUsAcUrl());
        assertEquals(expected.isReleaseCalendar(), actual.isReleaseCalendar());
        assertEquals(expected.getReleaseCalendarAccess(), actual.getReleaseCalendarAccess());
        // TODO UPDATE_FREQUENCY
        // TODO CURRENT_INTERNAL_INSTANCE
        // TODO CURRENT_INSTANCE
        assertEquals(expected.getInventoryDate(), actual.getInventoryDate());
        assertEqualsInternationalString(expected.getRevPolicy(), actual.getRevPolicy());
        assertEquals(expected.getRevPolicyUrl(), actual.getRevPolicyUrl());
        assertEqualsInternationalString(expected.getRevPractice(), actual.getRevPractice());
        assertEquals(expected.getRevPracticeUrl(), actual.getRevPracticeUrl());
        // TODO LEGAL_ACTS
        // TODO DATA_SHARING
        // TODO CONFIDENTIALITY_POLICY
        // TODO CONFIDENTIALITY_DATA_TREATMENT
        assertEqualsInternationalString(expected.getComment(), actual.getComment());
        assertEquals(expected.getCommentUrl(), actual.getCommentUrl());
        assertEqualsInternationalString(expected.getNotes(), actual.getNotes());
        assertEquals(expected.getNotesUrl(), actual.getNotesUrl());
    }

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
    
    // TODO pasar a librería de Asserts
    private static void assertEqualsNullability(Object expected, Object actual) {
        if (actual == null && expected == null) {
            return;
        } else if ((actual != null && expected == null) || (actual == null && expected != null)) {
            fail();
        }
    }
}
