package org.siemac.metamac.statistical.operations.core.utils.mocks;

import org.siemac.metamac.common.test.utils.MetamacMocks;
import org.siemac.metamac.core.common.ent.domain.ExternalItem;
import org.siemac.metamac.core.common.ent.domain.InternationalString;
import org.siemac.metamac.core.common.ent.domain.LocalisedString;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.core.common.util.GeneratorUrnUtils;
import org.siemac.metamac.core.common.util.shared.VersionUtil;

public abstract class StatisticalOperationsMocks extends MetamacMocks {

    private static final String[] MAINTAINER_MOCK          = new String[]{"MaintainerMock"};
    private static final String   ORGANIZATION_UNIT_MOCK   = "OrganizationUnitMock";
    private static final String   AGENCY_SCHEME_MOCK       = "AgencySchemeMock";
    private static final String   CONCEPT_SCHEME_MOCK      = "ConceptSchemeMock";
    private static final String   CODELIST_MOCK            = "CodelistMock";
    private static final String   DSD_MOCK                 = "DsdMock";

    private static final String   URI_MOCK_PREFIX          = "lorem/ipsum/dolor/sit/amet/";

    // -----------------------------------------------------------------
    // INTERNATIONAL STRING
    // -----------------------------------------------------------------

    public static InternationalString mockInternationalString() {
        InternationalString internationalString = new InternationalString();
        LocalisedString es = new LocalisedString();
        es.setLabel(mockString(10) + " en Espanol");
        es.setLocale("es");
        es.setVersion(Long.valueOf(0));
        LocalisedString en = new LocalisedString();
        en.setLabel(mockString(10) + " in English");
        en.setLocale("en");
        en.setVersion(Long.valueOf(0));
        internationalString.addText(es);
        internationalString.addText(en);
        internationalString.setVersion(Long.valueOf(0));
        return internationalString;
    }

    /**
     * Mock an InternationalString with one locale
     */
    public static InternationalString mockInternationalString(String locale, String label) {
        InternationalString target = new InternationalString();
        LocalisedString localisedString = new LocalisedString();
        localisedString.setLocale(locale);
        localisedString.setLabel(label);
        target.addText(localisedString);
        return target;
    }

    /**
     * Mock an InternationalString with two locales
     */
    public static InternationalString mockInternationalString(String locale01, String label01, String locale02, String label02) {
        InternationalString target = new InternationalString();
        LocalisedString localisedString01 = new LocalisedString();
        localisedString01.setLocale(locale01);
        localisedString01.setLabel(label01);
        target.addText(localisedString01);

        LocalisedString localisedString02 = new LocalisedString();
        localisedString02.setLocale(locale02);
        localisedString02.setLabel(label02);
        target.addText(localisedString02);
        return target;
    }

    // -----------------------------------------------------------------
    // EXTERNAL ITEM
    // -----------------------------------------------------------------

    public static ExternalItem mockStatisticalOperationItem() {
        String code = mockCode();
        ExternalItem item = new ExternalItem(code, getUriMock(), mockStatisticalOperationUrn(code), TypeExternalArtefactsEnum.STATISTICAL_OPERATION);
        item.setVersion(Long.valueOf(0));
        return item;
    }

    public static ExternalItem mockStatisticalOperationItem(String code) {
        ExternalItem item = new ExternalItem(code, getUriMock(), mockStatisticalOperationUrn(code), TypeExternalArtefactsEnum.STATISTICAL_OPERATION);
        item.setVersion(Long.valueOf(0));
        return item;
    }

    public static ExternalItem mockStatisticalOperationInstanceItem() {
        String code = mockCode();
        ExternalItem item = new ExternalItem(code, getUriMock(), mockStatisticalOperationInstanceUrn(code), TypeExternalArtefactsEnum.STATISTICAL_OPERATION_INSTANCE);
        item.setVersion(Long.valueOf(0));
        return item;
    }

    public static ExternalItem mockAgencyExternalItem() {
        String code = mockCode();
        ExternalItem item = new ExternalItem(code, getUriMock(), mockAgencyUrn(code), TypeExternalArtefactsEnum.AGENCY);
        item.setVersion(Long.valueOf(0));
        return item;
    }

    public static ExternalItem mockOrganizationUnitExternalItem() {
        String code = mockCode();
        ExternalItem item = new ExternalItem(code, getUriMock(), mockAgencyUrn(code), TypeExternalArtefactsEnum.ORGANISATION_UNIT);
        item.setVersion(Long.valueOf(0));
        return item;
    }

    public static ExternalItem mockConceptExternalItem() {
        String code = mockCode();
        ExternalItem item = new ExternalItem(code, getUriMock(), mockConceptUrn(code), TypeExternalArtefactsEnum.CONCEPT);
        item.setVersion(Long.valueOf(0));
        return item;
    }

    public static ExternalItem mockConceptSchemeExternalItem() {
        String code = mockCode();
        ExternalItem item = new ExternalItem(code, getUriMock(), mockConceptSchemeUrn(code), TypeExternalArtefactsEnum.CONCEPT_SCHEME);
        item.setVersion(Long.valueOf(0));
        return item;
    }

    public static ExternalItem mockCodeListSchemeExternalItem() {
        String code = mockCode();
        ExternalItem item = new ExternalItem(code, getUriMock(), mockCodeListUrn(code), TypeExternalArtefactsEnum.CODELIST);
        item.setVersion(Long.valueOf(0));
        return item;
    }

    public static ExternalItem mockCodeExternalItem() {
        String code = mockCode();
        ExternalItem item = new ExternalItem(code, getUriMock(), mockCodeUrn(code), TypeExternalArtefactsEnum.CODE);
        item.setVersion(Long.valueOf(0));
        return item;
    }

    public static ExternalItem mockDsdExternalItem() {
        String code = mockCode();
        ExternalItem item = new ExternalItem(code, getUriMock(), mockDsdUrn(code), TypeExternalArtefactsEnum.DATASTRUCTURE);
        item.setVersion(Long.valueOf(0));
        return item;
    }

    public static ExternalItem mockDimensionExternalItem() {
        String code = mockCode();
        ExternalItem item = new ExternalItem(code, getUriMock(), mockDimensionUrn(code), TypeExternalArtefactsEnum.DIMENSION);
        item.setVersion(Long.valueOf(0));
        return item;
    }

    private static String mockCode() {
        return mockString(8);
    }

    public static String mockStatisticalOperationUrn(String code) {
        return GeneratorUrnUtils.generateSiemacStatisticalOperationUrn(code);
    }

    public static String mockStatisticalOperationInstanceUrn(String code) {
        return GeneratorUrnUtils.generateSiemacStatisticalOperationUrn(code);
    }

    public static String mockAgencyUrn(String code) {
        return GeneratorUrnUtils.generateSdmxAgencyUrn(MAINTAINER_MOCK, AGENCY_SCHEME_MOCK, VersionUtil.PATTERN_X_Y_INITIAL_VERSION, code);
    }

    public static String mockOrganizationUnitUrn(String code) {
        return GeneratorUrnUtils.generateSdmxOrganisationUnitUrn(MAINTAINER_MOCK, ORGANIZATION_UNIT_MOCK, VersionUtil.PATTERN_X_Y_INITIAL_VERSION, code);
    }

    public static String mockConceptUrn(String code) {
        return GeneratorUrnUtils.generateSdmxConceptUrn(MAINTAINER_MOCK, CONCEPT_SCHEME_MOCK, VersionUtil.PATTERN_X_Y_INITIAL_VERSION, code);
    }

    public static String mockConceptSchemeUrn(String code) {
        return GeneratorUrnUtils.generateSdmxConceptSchemeUrn(MAINTAINER_MOCK, code, VersionUtil.PATTERN_X_Y_INITIAL_VERSION);
    }

    public static String mockCodeListUrn(String code) {
        return GeneratorUrnUtils.generateSdmxCodelistUrn(MAINTAINER_MOCK, code, VersionUtil.PATTERN_X_Y_INITIAL_VERSION);
    }

    public static String mockCodeUrn(String code) {
        return GeneratorUrnUtils.generateSdmxCodeUrn(MAINTAINER_MOCK, CODELIST_MOCK, VersionUtil.PATTERN_X_Y_INITIAL_VERSION, code);
    }

    public static String mockDsdUrn(String code) {
        return GeneratorUrnUtils.generateSdmxDatastructureUrn(MAINTAINER_MOCK, code, VersionUtil.PATTERN_X_Y_INITIAL_VERSION);
    }

    public static String mockDimensionUrn(String code) {
        return GeneratorUrnUtils.generateSdmxDimensionUrn(MAINTAINER_MOCK, DSD_MOCK, VersionUtil.PATTERN_XX_YYY_INITIAL_VERSION, code);
    }

    // -----------------------------------------------------------------
    // PRIVATE
    // -----------------------------------------------------------------
    private static String getUriMock() {
        return URI_MOCK_PREFIX + mockString(5);
    }

}
