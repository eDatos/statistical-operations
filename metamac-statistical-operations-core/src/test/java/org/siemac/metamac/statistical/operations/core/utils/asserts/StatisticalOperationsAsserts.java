package org.siemac.metamac.statistical.operations.core.utils.asserts;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.time.DateUtils;
import org.junit.Assert;
import org.siemac.metamac.common.test.constants.ConfigurationMockConstants;
import org.siemac.metamac.common.test.utils.MetamacAsserts;
import org.siemac.metamac.core.common.constants.shared.RegularExpressionConstants;
import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.core.common.dto.InternationalStringDto;
import org.siemac.metamac.core.common.ent.domain.ExternalItem;
import org.siemac.metamac.core.common.ent.domain.InternationalString;
import org.siemac.metamac.core.common.ent.domain.LocalisedString;
import org.siemac.metamac.core.common.enume.utils.TypeExternalArtefactsEnumUtils;
import org.siemac.metamac.statistical.operations.core.dto.FamilyDto;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import static org.siemac.metamac.core.common.constants.CoreCommonConstants.API_LATEST;

public class StatisticalOperationsAsserts extends MetamacAsserts {

    // -----------------------------------------------------------------
    // FAMILY: DTO & DTO
    // -----------------------------------------------------------------

    public static void assertEqualsFamilyDto(FamilyDto expected, FamilyDto actual) {
        assertEqualsNullability(expected, actual);
        if (expected == null) {
            return;
        }
        assertEqualsInternationalStringDto(expected.getTitle(), actual.getTitle());
        assertEqualsInternationalStringDto(expected.getDescription(), actual.getDescription());
        assertEqualsInternationalStringDto(expected.getAcronym(), actual.getAcronym());
        assertEquals(expected.getCode(), actual.getCode());
        assertEqualsDate(expected.getInternalInventoryDate(), actual.getInventoryDate());
        assertEqualsDate(expected.getInventoryDate(), actual.getInventoryDate());
        assertEquals(expected.getProcStatus(), actual.getProcStatus());
        assertEquals(expected.getUrn(), actual.getUrn());
    }

    private static void assertEqualsDate(Date expected, Date actual) {
        assertEqualsNullability(expected, actual);
        if (expected == null) {
            return;
        }

        DateUtils.isSameInstant(expected, actual);
    }

    // -----------------------------------------------------------------
    // EXTERNAL ITEMS: DO & DO
    // -----------------------------------------------------------------

    public static void assertEqualsExternalItem(ExternalItem expected, ExternalItem actual) {

        assertEqualsNullability(expected, actual);
        if (expected == null) {
            return;
        }

        assertEquals(expected.getCode(), actual.getCode());
        assertEquals(expected.getCodeNested(), actual.getCodeNested());
        assertEquals(expected.getUri(), actual.getUri());
        assertEquals(expected.getUrn(), actual.getUrn());
        assertEquals(expected.getUrnProvider(), actual.getUrnProvider());
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
                if (expec.getUrn() != null) {
                    if (expec.getUrn().equals(actualItem.getUrn())) {
                        found = true;
                    }
                }
                if (!found) {
                    if (expec.getUrnProvider() != null) {
                        if (expec.getUrnProvider().equals(actualItem.getUrnProvider())) {
                            found = true;
                        }
                    }
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

    public static void assertEqualsExternalItem(ExternalItem entity, ExternalItemDto dto, MapperEnum mapperEnum) {
        assertEqualsNullability(entity, dto);
        if (entity == null) {
            return;
        }

        String baseApi = null;
        String baseWebApplication = null;

        if (TypeExternalArtefactsEnumUtils.isExternalItemOfCommonMetadataApp(dto.getType())) {
            baseWebApplication = ConfigurationMockConstants.COMMON_METADATA_INTERNAL_WEB_APP_URL_BASE;
            baseApi = ConfigurationMockConstants.COMMON_METADATA_EXTERNAL_API_URL_BASE;
        } else if (TypeExternalArtefactsEnumUtils.isExternalItemOfStatisticalOperationsApp(dto.getType())) {
            baseWebApplication = ConfigurationMockConstants.STATISTICAL_OPERATIONS_INTERNAL_WEB_APP_URL_BASE;
            baseApi = ConfigurationMockConstants.STATISTICAL_OPERATIONS_INTERNAL_API_URL_BASE;
        } else if (TypeExternalArtefactsEnumUtils.isExternalItemOfSrmApp(dto.getType())) {
            baseWebApplication = ConfigurationMockConstants.SRM_INTERNAL_WEB_APP_URL_BASE;
            baseApi = ConfigurationMockConstants.SRM_INTERNAL_API_URL_BASE;
        } else {
            fail("unexpected type of external item");
        }

        assertEqualsExternalItem(entity, dto, baseApi, baseWebApplication, mapperEnum);
    }

    private static void assertEqualsExternalItem(ExternalItem entity, ExternalItemDto dto, String baseApi, String baseWebApplication, MapperEnum mapperEnum) {
        assertEqualsExternalItem(entity, dto);

        assertEqualsNullability(entity.getUri(), dto.getUri());
        if (entity.getUri() != null) {
            if (MapperEnum.DO2DTO.equals(mapperEnum)) {
                assertEquals(baseApi + entity.getUri(), dto.getUri());
            } else if (MapperEnum.DTO2DO.equals(mapperEnum)) {
                String expectedDoUri = dto.getUri().replaceFirst(baseApi, StringUtils.EMPTY);
                expectedDoUri = expectedDoUri.replaceFirst(RegularExpressionConstants.API_VERSION_REG_EXP, API_LATEST);
                assertEquals(expectedDoUri, entity.getUri());
            } else {
                fail("Mapper unexpected: " + mapperEnum);
            }
        }

        assertEqualsNullability(entity.getManagementAppUrl(), dto.getManagementAppUrl());
        if (entity.getManagementAppUrl() != null) {
            if (MapperEnum.DO2DTO.equals(mapperEnum)) {
                assertEquals(baseWebApplication + entity.getManagementAppUrl(), dto.getManagementAppUrl());
            } else if (MapperEnum.DTO2DO.equals(mapperEnum)) {
                assertEquals(dto.getManagementAppUrl().replaceFirst(baseWebApplication, StringUtils.EMPTY), entity.getManagementAppUrl());
            } else {
                fail("Mapper unexpected: " + mapperEnum);
            }
        }
    }

    private static void assertEqualsExternalItem(ExternalItem entity, ExternalItemDto dto) {
        assertEquals(entity.getCode(), dto.getCode());
        assertEquals(entity.getCodeNested(), dto.getCodeNested());
        assertEquals(entity.getUrn(), dto.getUrn());
        assertEquals(entity.getUrnProvider(), dto.getUrnProvider());
        assertEquals(entity.getType(), dto.getType());
        assertEqualsInternationalString(entity.getTitle(), dto.getTitle());
    }

    public static void assertEqualsExternalItemCollectionMapper(Collection<ExternalItem> entities, Collection<ExternalItemDto> dtos, MapperEnum mapperEnum) {
        if (entities == null) {
            entities = new ArrayList<ExternalItem>();
        }

        if (dtos == null) {
            dtos = new ArrayList<ExternalItemDto>();
        }

        assertEquals(entities.size(), dtos.size());
        for (ExternalItem entity : entities) {
            boolean found = false;
            Iterator<ExternalItemDto> itDto = dtos.iterator();
            while (itDto.hasNext() && !found) {
                ExternalItemDto dto = itDto.next();
                found = true;
                try {
                    assertEqualsExternalItem(entity, dto, mapperEnum);
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
