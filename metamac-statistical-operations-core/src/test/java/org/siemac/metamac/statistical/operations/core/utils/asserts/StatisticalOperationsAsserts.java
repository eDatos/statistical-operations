package org.siemac.metamac.statistical.operations.core.utils.asserts;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.junit.Assert;
import org.siemac.metamac.common.test.utils.MetamacAsserts;
import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.core.common.dto.InternationalStringDto;
import org.siemac.metamac.core.common.ent.domain.ExternalItem;
import org.siemac.metamac.core.common.ent.domain.InternationalString;
import org.siemac.metamac.core.common.ent.domain.LocalisedString;

public class StatisticalOperationsAsserts extends MetamacAsserts {

    // -----------------------------------------------------------------
    // EXTERNAL ITEMS: DO & DO
    // -----------------------------------------------------------------

    public static void assertEqualsExternalItem(ExternalItem expected, ExternalItem actual) {

        assertEqualsNullability(expected, actual);
        if (expected == null) {
            return;
        }

        assertEquals(expected.getCode(), actual.getCode());
        assertEquals(expected.getUri(), actual.getUri());
        assertEquals(expected.getUrn(), actual.getUrn());
        assertEquals(expected.getType(), actual.getType());
        assertEqualsInternationalString(expected.getTitle(), actual.getTitle());
        assertEquals(expected.getManagementAppUrl(), actual.getManagementAppUrl());
    }

    // CAUTION: objects must implement equals!
    public static void assertEqualsExternalItemCollection(Collection<ExternalItem> expected, Collection<ExternalItem> actual) {
        assertEqualsNullability(expected, actual);
        if (expected == null) {
            return;
        }
        assertEquals(expected.size(), actual.size());

        for (ExternalItem expec : expected) {
            boolean found = false;
            for (ExternalItem actualItem : actual) {
                if (actualItem.getUrn().equals(expec.getUrn())) {
                    found = true;
                }
            }
            if (!found) {
                Assert.fail("Found element in expected collection which is not contained in actual collection");
            }
        }
    }

    public static void assertEqualsExternalItemList(List<ExternalItem> expected, List<ExternalItem> actual) {
        assertEqualsNullability(expected, actual);
        if (expected == null) {
            return;
        }
        assertEquals(expected.size(), actual.size());

        for (int i = 0; i < expected.size(); i++) {
            assertEqualsExternalItem(expected.get(i), actual.get(i));
        }
    }

    // -----------------------------------------------------------------
    // EXTERNAL ITEMS: DTO & DO
    // -----------------------------------------------------------------

    public static void assertEqualsExternalItem(ExternalItem entity, ExternalItemDto dto) {

        assertEqualsNullability(entity, dto);
        if (entity == null) {
            return;
        }

        assertEquals(entity.getCode(), dto.getCode());
        assertEquals(entity.getUri(), dto.getUri());
        assertEquals(entity.getUrn(), dto.getUrn());
        assertEquals(entity.getType(), dto.getType());
        assertEqualsInternationalString(entity.getTitle(), dto.getTitle());
        assertEquals(entity.getManagementAppUrl(), dto.getManagementAppUrl());
    }

    public static void assertEqualsExternalItemCollectionMapper(Collection<ExternalItem> entities, Collection<ExternalItemDto> dtos) {

        assertEqualsNullability(entities, dtos);
        if (entities == null) {
            assertNull(dtos);
        }
        assertEquals(entities.size(), dtos.size());
        for (ExternalItem entity : entities) {
            boolean found = false;
            Iterator<ExternalItemDto> itDto = dtos.iterator();
            while (itDto.hasNext() && !found) {
                ExternalItemDto dto = itDto.next();
                found = true;
                try {
                    assertEqualsExternalItem(entity, dto);
                } catch (AssertionError e) {
                    found = false;
                }
            }
            if (!found) {
                Assert.fail("Not equal collections");
            }
        }
    }
    
    // -----------------------------------------------------------------
    // INTERNATIONAL STRING: DO & DO
    // -----------------------------------------------------------------

    public static void assertEqualsInternationalString(InternationalString expected, InternationalString actual) {
        assertEqualsNullability(expected, actual);
        if (expected == null) {
            return;
        }

        assertEquals(expected.getTexts().size(), actual.getTexts().size());
        for (LocalisedString localisedStringExpected : expected.getTexts()) {
            assertEquals(localisedStringExpected.getLabel(), actual.getLocalisedLabel(localisedStringExpected.getLocale()));
        }
    }

    public static void assertEqualsInternationalString(InternationalString internationalString, String locale1, String label1, String locale2, String label2) {
        int count = 0;
        if (locale1 != null) {
            assertEquals(label1, internationalString.getLocalisedLabel(locale1));
            count++;
        }
        if (locale2 != null) {
            assertEquals(label2, internationalString.getLocalisedLabel(locale2));
            count++;
        }
        assertEquals(count, internationalString.getTexts().size());
    }

    // -----------------------------------------------------------------
    // INTERNATIONAL STRING: DTO & DO
    // -----------------------------------------------------------------

    public static void assertEqualsInternationalString(InternationalString entity, InternationalStringDto dto) {
        assertEqualsNullability(entity, dto);
        if (entity == null) {
            return;
        }

        assertEquals(entity.getTexts().size(), dto.getTexts().size());
        for (LocalisedString localisedStringExpected : entity.getTexts()) {
            assertEquals(localisedStringExpected.getLabel(), dto.getLocalisedLabel(localisedStringExpected.getLocale()));
        }
    }

}
