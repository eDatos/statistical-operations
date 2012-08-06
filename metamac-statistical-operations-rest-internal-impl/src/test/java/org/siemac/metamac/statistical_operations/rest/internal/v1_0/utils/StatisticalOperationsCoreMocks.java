package org.siemac.metamac.statistical_operations.rest.internal.v1_0.utils;

import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.List;

import org.fornax.cartridges.sculptor.framework.domain.PagedResult;
import org.joda.time.DateTime;
import org.siemac.metamac.core.common.ent.domain.ExternalItem;
import org.siemac.metamac.core.common.ent.domain.InternationalString;
import org.siemac.metamac.core.common.ent.domain.LocalisedString;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.core.common.util.GeneratorUrnUtils;
import org.siemac.metamac.statistical.operations.core.domain.CollMethod;
import org.siemac.metamac.statistical.operations.core.domain.Cost;
import org.siemac.metamac.statistical.operations.core.domain.Family;
import org.siemac.metamac.statistical.operations.core.domain.Instance;
import org.siemac.metamac.statistical.operations.core.domain.InstanceType;
import org.siemac.metamac.statistical.operations.core.domain.OfficialityType;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.core.domain.SurveySource;
import org.siemac.metamac.statistical.operations.core.domain.SurveyType;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.core.enume.domain.StatusEnum;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.service.StatisticalOperationsRestInternalFacadeV10Test;

public class StatisticalOperationsCoreMocks {

    public static Operation mockOperation1() {
        return mockOperation("1", ProcStatusEnum.PUBLISH_INTERNALLY, Boolean.TRUE, mockFamily1RelatedEntity(), mockFamily2RelatedEntity());
    }

    public static Operation mockOperation2() {
        return mockOperation("2", ProcStatusEnum.PUBLISH_INTERNALLY, Boolean.TRUE, mockFamily1RelatedEntity());
    }

    public static Operation mockOperation3() {
        return mockOperation("3", ProcStatusEnum.PUBLISH_EXTERNALLY, Boolean.TRUE, mockFamily1RelatedEntity());
    }

    public static Operation mockOperation4() {
        return mockOperation("4", ProcStatusEnum.PUBLISH_INTERNALLY, Boolean.TRUE, mockFamily1RelatedEntity());
    }

    public static Operation mockOperation5() {
        return mockOperation("5", ProcStatusEnum.PUBLISH_INTERNALLY, Boolean.TRUE, mockFamily1RelatedEntity());
    }

    public static Operation mockOperation6() {
        return mockOperation("6", ProcStatusEnum.PUBLISH_EXTERNALLY, Boolean.TRUE, mockFamily1RelatedEntity());
    }

    public static Operation mockOperation7() {
        return mockOperation("7", ProcStatusEnum.PUBLISH_EXTERNALLY, Boolean.TRUE, mockFamily2RelatedEntity());
    }

    public static Operation mockOperation8() {
        return mockOperation("8", ProcStatusEnum.PUBLISH_EXTERNALLY, Boolean.TRUE, mockFamily2RelatedEntity());
    }

    public static Operation mockOperation9() {
        return mockOperation("9", ProcStatusEnum.PUBLISH_EXTERNALLY, Boolean.TRUE, mockFamily2RelatedEntity());
    }

    public static Operation mockOperation10() {
        return mockOperation("10", ProcStatusEnum.PUBLISH_EXTERNALLY, Boolean.FALSE, mockFamily2RelatedEntity());
    }

    public static Family mockFamily1() {
        return mockFamily("1", ProcStatusEnum.PUBLISH_INTERNALLY);
    }

    public static Family mockFamily2() {
        return mockFamily("2", ProcStatusEnum.PUBLISH_EXTERNALLY);
    }

    public static Family mockFamily3() {
        return mockFamily("3", ProcStatusEnum.PUBLISH_EXTERNALLY);
    }

    public static Family mockFamily4() {
        return mockFamily("4", ProcStatusEnum.PUBLISH_EXTERNALLY);
    }

    public static Family mockFamily15() {
        return mockFamily("15", ProcStatusEnum.PUBLISH_EXTERNALLY);
    }

    public static Instance mockInstance1() {
        return mockInstance("1", ProcStatusEnum.PUBLISH_INTERNALLY);
    }
    public static Instance mockInstance2() {
        return mockInstance("2", ProcStatusEnum.PUBLISH_INTERNALLY);
    }
    public static Instance mockInstance3() {
        return mockInstance("3", ProcStatusEnum.PUBLISH_INTERNALLY);
    }
    public static Instance mockInstance4() {
        return mockInstance("4", ProcStatusEnum.PUBLISH_INTERNALLY);
    }
    public static Instance mockInstance15() {
        return mockInstance("15", ProcStatusEnum.PUBLISH_INTERNALLY);
    }

    public static PagedResult<Operation> mockOperationsPagedResult(String limit, String offset) {

        List<Operation> operations = new ArrayList<Operation>();
        int total = 10;
        int startRow = -1;
        int rowCount = -1;
        int pageSize = -1;
        if ((limit == null || "1000".equals(limit) || "25".equals(limit)) && (offset == null || "0".equals(offset))) {
            startRow = 0;
            rowCount = total;
            pageSize = total;
            operations.add(mockOperation1());
            operations.add(mockOperation2());
            operations.add(mockOperation3());
            operations.add(mockOperation4());
            operations.add(mockOperation5());
            operations.add(mockOperation6());
            operations.add(mockOperation7());
            operations.add(mockOperation8());
            operations.add(mockOperation9());
            operations.add(mockOperation10());
        } else if ("2".equals(limit) && "0".equals(offset)) {
            pageSize = Integer.valueOf(limit).intValue();
            startRow = Integer.valueOf(offset).intValue();
            rowCount = pageSize;
            operations.add(mockOperation1());
            operations.add(mockOperation2());
        } else if ("2".equals(limit) && "2".equals(offset)) {
            pageSize = Integer.valueOf(limit).intValue();
            startRow = Integer.valueOf(offset).intValue();
            rowCount = pageSize;
            operations.add(mockOperation3());
            operations.add(mockOperation4());
        } else if ("2".equals(limit) && "8".equals(offset)) {
            pageSize = Integer.valueOf(limit).intValue();
            startRow = Integer.valueOf(offset).intValue();
            rowCount = pageSize;
            operations.add(mockOperation9());
            operations.add(mockOperation10());
        } else if ("2".equals(limit) && "9".equals(offset)) {
            pageSize = Integer.valueOf(limit).intValue();
            startRow = Integer.valueOf(offset).intValue();
            // no results
            rowCount = 0;
        } else {
            fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
        }

        return new PagedResult<Operation>(operations, startRow, rowCount, pageSize, total, -1);
    }

    public static PagedResult<Operation> mockOperationsPagedResult(String limit, String offset, String query) {

        // Without query
        if (query == null) {
            return mockOperationsPagedResult(limit, offset);
        }

        // With query
        List<Operation> operations = new ArrayList<Operation>();
        int total = -1;
        int startRow = -1;
        int rowCount = -1;
        int pageSize = -1;

        String querySupported1 = StatisticalOperationsRestInternalFacadeV10Test.QUERY_OPERATION_ID_LIKE_1;
        String querySupported2 = StatisticalOperationsRestInternalFacadeV10Test.QUERY_OPERATION_ID_LIKE_1_AND_INDICATORS_SYSTEM;
        if (querySupported1.equals(query)) {
            total = 2;
            startRow = -1;
            rowCount = -1;
            pageSize = -1;
            if ((limit == null || "1000".equals(limit) || "25".equals(limit)) && (offset == null || "0".equals(offset))) {
                startRow = 0;
                rowCount = total;
                pageSize = total;
                operations.add(mockOperation1());
                operations.add(mockOperation10());
            } else if ("1".equals(limit) && "0".equals(offset)) {
                pageSize = Integer.valueOf(limit).intValue();
                startRow = Integer.valueOf(offset).intValue();
                rowCount = pageSize;
                operations.add(mockOperation1());
            } else if ("1".equals(limit) && "1".equals(offset)) {
                pageSize = Integer.valueOf(limit).intValue();
                startRow = Integer.valueOf(offset).intValue();
                rowCount = pageSize;
                operations.add(mockOperation10());
            } else {
                fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
            }
        } else if (querySupported2.equals(query)) {
            total = 1;
            startRow = -1;
            rowCount = -1;
            pageSize = -1;
            if ((limit == null || "1000".equals(limit) || "25".equals(limit)) && (offset == null || "0".equals(offset))) {
                startRow = 0;
                rowCount = total;
                pageSize = total;
                operations.add(mockOperation1());
            } else {
                fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
            }
        } else {
            fail("Query not supported = " + query);
        }
        return new PagedResult<Operation>(operations, startRow, rowCount, pageSize, total, -1);
    }

    public static PagedResult<Operation> mockOperationsPagedResultByFamily1(String limit, String offset) {

        List<Operation> operations = new ArrayList<Operation>();
        int total = 6;
        int startRow = -1;
        int rowCount = -1;
        int pageSize = -1;
        if ((limit == null || "1000".equals(limit) || "25".equals(limit)) && (offset == null || "0".equals(offset))) {
            startRow = 0;
            rowCount = total;
            pageSize = total;
            operations.add(mockOperation1());
            operations.add(mockOperation2());
            operations.add(mockOperation3());
            operations.add(mockOperation4());
            operations.add(mockOperation5());
            operations.add(mockOperation6());
        } else if ("2".equals(limit) && "0".equals(offset)) {
            pageSize = Integer.valueOf(limit).intValue();
            startRow = Integer.valueOf(offset).intValue();
            rowCount = pageSize;
            operations.add(mockOperation1());
            operations.add(mockOperation2());
        } else if ("2".equals(limit) && "2".equals(offset)) {
            pageSize = Integer.valueOf(limit).intValue();
            startRow = Integer.valueOf(offset).intValue();
            rowCount = pageSize;
            operations.add(mockOperation3());
            operations.add(mockOperation4());
        } else if ("2".equals(limit) && "4".equals(offset)) {
            pageSize = Integer.valueOf(limit).intValue();
            startRow = Integer.valueOf(offset).intValue();
            rowCount = pageSize;
            operations.add(mockOperation5());
            operations.add(mockOperation6());
        } else if ("2".equals(limit) && "7".equals(offset)) {
            pageSize = Integer.valueOf(limit).intValue();
            startRow = Integer.valueOf(offset).intValue();
            // no results
            rowCount = 0;
        } else {
            fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
        }

        return new PagedResult<Operation>(operations, startRow, rowCount, pageSize, total, -1);
    }

    public static PagedResult<Operation> mockOperationsPagedResultByFamily2(String limit, String offset) {

        List<Operation> operations = new ArrayList<Operation>();
        int total = 4;
        int startRow = -1;
        int rowCount = -1;
        int pageSize = -1;
        if ("1".equals(limit) && "2".equals(offset)) {
            pageSize = Integer.valueOf(limit).intValue();
            startRow = Integer.valueOf(offset).intValue();
            rowCount = pageSize;
            operations.add(mockOperation8());
        } else {
            fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
        }

        return new PagedResult<Operation>(operations, startRow, rowCount, pageSize, total, -1);
    }

    public static PagedResult<Family> mockFamiliesPagedResult(String limit, String offset) {

        List<Family> families = new ArrayList<Family>();
        int total = 5;
        int startRow = -1;
        int rowCount = -1;
        int pageSize = -1;
        if ((limit == null || "1000".equals(limit) || "25".equals(limit)) && (offset == null || "0".equals(offset))) {
            startRow = 0;
            rowCount = total;
            pageSize = total;
            families.add(mockFamily1());
            families.add(mockFamily2());
            families.add(mockFamily3());
            families.add(mockFamily4());
            families.add(mockFamily15());
        } else if ("2".equals(limit) && "0".equals(offset)) {
            pageSize = Integer.valueOf(limit).intValue();
            startRow = Integer.valueOf(offset).intValue();
            rowCount = pageSize;
            families.add(mockFamily1());
            families.add(mockFamily2());
        } else if ("2".equals(limit) && "2".equals(offset)) {
            pageSize = Integer.valueOf(limit).intValue();
            startRow = Integer.valueOf(offset).intValue();
            rowCount = pageSize;
            families.add(mockFamily3());
            families.add(mockFamily4());
        } else if ("2".equals(limit) && "4".equals(offset)) {
            pageSize = Integer.valueOf(limit).intValue();
            startRow = Integer.valueOf(offset).intValue();
            rowCount = pageSize;
            families.add(mockFamily15());
        } else if ("2".equals(limit) && "9".equals(offset)) {
            pageSize = Integer.valueOf(limit).intValue();
            startRow = Integer.valueOf(offset).intValue();
            // no results
            rowCount = 0;
        } else {
            fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
        }

        return new PagedResult<Family>(families, startRow, rowCount, pageSize, total, -1);
    }

    public static PagedResult<Family> mockFamiliesPagedResult(String limit, String offset, String query) {

        // No queries
        if (query == null) {
            return mockFamiliesPagedResult(limit, offset);
        }

        // With queries
        int total = -1;
        int startRow = -1;
        int rowCount = -1;
        int pageSize = -1;
        List<Family> families = new ArrayList<Family>();

        String querySupported1 = StatisticalOperationsRestInternalFacadeV10Test.QUERY_FAMILY_ID_LIKE_1;
        if (querySupported1.equals(query)) {
            total = 2;
            startRow = -1;
            rowCount = -1;
            pageSize = -1;
            if ((limit == null || "1000".equals(limit) || "25".equals(limit)) && (offset == null || "0".equals(offset))) {
                startRow = 0;
                rowCount = total;
                pageSize = total;
                families.add(mockFamily1());
                families.add(mockFamily15());
            } else if ("1".equals(limit) && "0".equals(offset)) {
                pageSize = Integer.valueOf(limit).intValue();
                startRow = Integer.valueOf(offset).intValue();
                rowCount = pageSize;
                families.add(mockFamily1());
            } else if ("1".equals(limit) && "1".equals(offset)) {
                pageSize = Integer.valueOf(limit).intValue();
                startRow = Integer.valueOf(offset).intValue();
                rowCount = pageSize;
                families.add(mockFamily15());
            } else {
                fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
            }
        } else {
            fail("Query non supported = " + query);
        }

        return new PagedResult<Family>(families, startRow, rowCount, pageSize, total, -1);
    }

    public static PagedResult<Family> mockFamiliesNoPagedResultByOperation1() {

        List<Family> families = new ArrayList<Family>();
        families.add(mockFamily1());
        families.add(mockFamily2());

        int startRow = 0;
        int rowCount = 2;
        int pageSize = 2;
        int totalRows = 2;

        return new PagedResult<Family>(families, startRow, rowCount, pageSize, totalRows, -1);
    }

    public static PagedResult<Instance> mockInstancesPagedResultByOperation1(String limit, String offset) {

        List<Instance> instances = new ArrayList<Instance>();
        int total = 5;
        int startRow = -1;
        int rowCount = -1;
        int pageSize = -1;
        if ((limit == null || "1000".equals(limit) || "25".equals(limit)) && (offset == null || "0".equals(offset))) {
            startRow = 0;
            rowCount = total;
            pageSize = total;
            instances.add(mockInstance1());
            instances.add(mockInstance2());
            instances.add(mockInstance3());
            instances.add(mockInstance4());
            instances.add(mockInstance15());
        } else if ("2".equals(limit) && "0".equals(offset)) {
            pageSize = Integer.valueOf(limit).intValue();
            startRow = Integer.valueOf(offset).intValue();
            rowCount = pageSize;
            instances.add(mockInstance1());
            instances.add(mockInstance2());
        } else if ("2".equals(limit) && "2".equals(offset)) {
            pageSize = Integer.valueOf(limit).intValue();
            startRow = Integer.valueOf(offset).intValue();
            rowCount = pageSize;
            instances.add(mockInstance3());
            instances.add(mockInstance4());
        } else if ("2".equals(limit) && "4".equals(offset)) {
            pageSize = Integer.valueOf(limit).intValue();
            startRow = Integer.valueOf(offset).intValue();
            rowCount = pageSize;
            instances.add(mockInstance15());
        } else if ("2".equals(limit) && "9".equals(offset)) {
            pageSize = Integer.valueOf(limit).intValue();
            startRow = Integer.valueOf(offset).intValue();
            // no results
            rowCount = 0;
        } else {
            fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
        }

        return new PagedResult<Instance>(instances, startRow, rowCount, pageSize, total, -1);
    }

    public static PagedResult<Instance> mockInstancesPagedResultByOperation1(String limit, String offset, String query) {

        // No queries
        if (query == null) {
            return mockInstancesPagedResultByOperation1(limit, offset);
        }

        // With queries
        int total = -1;
        int startRow = -1;
        int rowCount = -1;
        int pageSize = -1;
        List<Instance> instances = new ArrayList<Instance>();

        String querySupported1 = StatisticalOperationsRestInternalFacadeV10Test.QUERY_FAMILY_ID_LIKE_1;
        if (querySupported1.equals(query)) {
            total = 2;
            startRow = -1;
            rowCount = -1;
            pageSize = -1;
            if ((limit == null || "1000".equals(limit) || "25".equals(limit)) && (offset == null || "0".equals(offset))) {
                startRow = 0;
                rowCount = total;
                pageSize = total;
                instances.add(mockInstance1());
                instances.add(mockInstance15());
            } else if ("1".equals(limit) && "0".equals(offset)) {
                pageSize = Integer.valueOf(limit).intValue();
                startRow = Integer.valueOf(offset).intValue();
                rowCount = pageSize;
                instances.add(mockInstance1());
            } else if ("1".equals(limit) && "1".equals(offset)) {
                pageSize = Integer.valueOf(limit).intValue();
                startRow = Integer.valueOf(offset).intValue();
                rowCount = pageSize;
                instances.add(mockInstance15());
            } else {
                fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
            }
        } else {
            fail("Query non supported = " + query);
        }

        return new PagedResult<Instance>(instances, startRow, rowCount, pageSize, total, -1);
    }

    /**
     * Operation with basic attributes. Do not use mockInstance to avoid cyclic method calls
     */
    private static Operation mockOperationRelatedEntity(String subCode, ProcStatusEnum procStatus) {
        Operation operation = new Operation();
        operation.setCode("operation" + subCode);
        operation.setUrn(GeneratorUrnUtils.generateSiemacStatisticalOperationUrn(operation.getCode()));
        operation.setTitle(mockInternationalString("operation", subCode));
        operation.setProcStatus(procStatus);
        operation.addInstance(mockInstanceRelatedEntity("4444", ProcStatusEnum.DRAFT, Integer.valueOf(0), operation.getCode()));
        operation.addInstance(mockInstanceRelatedEntity("22", ProcStatusEnum.PUBLISH_INTERNALLY, Integer.valueOf(3), operation.getCode()));
        operation.addInstance(mockInstanceRelatedEntity("1", ProcStatusEnum.PUBLISH_INTERNALLY, Integer.valueOf(2), operation.getCode()));
        operation.addInstance(mockInstanceRelatedEntity("333", ProcStatusEnum.PUBLISH_EXTERNALLY, Integer.valueOf(1), operation.getCode()));
        return operation;
    }

    private static Family mockFamily1RelatedEntity() {
        return mockFamilyRelatedEntity("1", ProcStatusEnum.PUBLISH_INTERNALLY);
    }

    private static Family mockFamily2RelatedEntity() {
        return mockFamilyRelatedEntity("2", ProcStatusEnum.PUBLISH_EXTERNALLY);
    }

    /**
     * Family with basic attributes. Do not use mockInstance to avoid cyclic method calls
     */
    private static Family mockFamilyRelatedEntity(String subCode, ProcStatusEnum procStatus) {
        Family family = new Family();
        family.setCode("family" + subCode);
        family.setUrn(GeneratorUrnUtils.generateSiemacStatisticalFamilyUrn(family.getCode()));
        family.setTitle(mockInternationalString("family", subCode));
        family.setProcStatus(procStatus);
        return family;
    }

    /**
     * Instance with basic attributes. Do not use mockInstance to avoid cyclic method calls
     */
    private static Instance mockInstanceRelatedEntity(String subCode, ProcStatusEnum procStatus, Integer order, String operationCode) {
        Instance instance = new Instance();
        instance.setCode("instance" + subCode);
        instance.setUrn(GeneratorUrnUtils.generateSiemacStatisticalOperationInstanceUrn(operationCode, instance.getCode()));
        instance.setTitle(mockInternationalString("instance", subCode));
        instance.setProcStatus(procStatus);
        instance.setOrder(order);
        return instance;
    }

    private static SurveyType mockSurveyType(String code) {
        SurveyType surveyType = new SurveyType();
        surveyType.setDescription(mockInternationalString(code, null));
        surveyType.setIdentifier(code);
        return surveyType;
    }

    private static OfficialityType mockOfficialityType(String code) {
        OfficialityType officialityType = new OfficialityType();
        officialityType.setDescription(mockInternationalString(code, null));
        officialityType.setIdentifier(code);
        return officialityType;
    }

    private static InstanceType mockInstanceType(String code) {
        InstanceType instanceType = new InstanceType();
        instanceType.setDescription(mockInternationalString(code, null));
        instanceType.setIdentifier(code);
        return instanceType;
    }

    private static SurveySource mockSurveySource(String code) {
        SurveySource surveySource = new SurveySource();
        surveySource.setDescription(mockInternationalString(code, null));
        surveySource.setIdentifier(code);
        return surveySource;
    }

    private static CollMethod mockCollMethod(String code) {
        CollMethod collMethod = new CollMethod();
        collMethod.setDescription(mockInternationalString(code, null));
        collMethod.setIdentifier(code);
        return collMethod;
    }

    private static Cost mockCost(String code) {
        Cost cost = new Cost();
        cost.setDescription(mockInternationalString(code, null));
        cost.setIdentifier(code);
        return cost;
    }

    private static ExternalItem mockExternalItem(String code, String subpathUrl, TypeExternalArtefactsEnum type) {
        String uri = subpathUrl + "/" + code;
        String urn = "urn:" + code;
        return new ExternalItem(code, uri, urn, type, mockInternationalString(code, null), null);
    }

    private static Operation mockOperation(String subCode, ProcStatusEnum procStatus, Boolean isIndicatorsSystem, Family... families) {

        Operation operation = new Operation();
        operation.setCode("operation" + subCode);
        operation.setUrn(GeneratorUrnUtils.generateSiemacStatisticalOperationUrn(operation.getCode()));
        operation.setTitle(mockInternationalString("operation", subCode));
        operation.setAcronym(mockInternationalString("acronym", subCode));
        if (families != null) {
            for (int i = 0; i < families.length; i++) {
                Family family = families[i];
                operation.addFamily(family);
            }
        }
        operation.setSubjectArea(mockExternalItem("subjectArea1", "subjectAreas", TypeExternalArtefactsEnum.CATEGORY));
        operation.getSecondarySubjectAreas().add(mockExternalItem("secundarySubjectArea1", "secundarySubjectAreas", TypeExternalArtefactsEnum.CATEGORY));
        operation.getSecondarySubjectAreas().add(mockExternalItem("secundarySubjectArea22", "secundarySubjectAreas", TypeExternalArtefactsEnum.CATEGORY));
        operation.getSecondarySubjectAreas().add(mockExternalItem("secundarySubjectArea333", "secundarySubjectAreas", TypeExternalArtefactsEnum.CATEGORY));
        operation.setObjective(mockInternationalString("objetive", subCode));
        operation.setDescription(mockInternationalString("description", subCode));
        operation.addInstance(mockInstanceRelatedEntity("4444", ProcStatusEnum.DRAFT, Integer.valueOf(0), operation.getCode()));
        operation.addInstance(mockInstanceRelatedEntity("22", ProcStatusEnum.PUBLISH_INTERNALLY, Integer.valueOf(3), operation.getCode()));
        operation.addInstance(mockInstanceRelatedEntity("1", ProcStatusEnum.PUBLISH_INTERNALLY, Integer.valueOf(2), operation.getCode()));
        operation.addInstance(mockInstanceRelatedEntity("333", ProcStatusEnum.PUBLISH_EXTERNALLY, Integer.valueOf(1), operation.getCode()));
        operation.setSurveyType(mockSurveyType("surveyIdentifier"));
        operation.setOfficialityType(mockOfficialityType("officialityType"));
        operation.setIndicatorSystem(isIndicatorsSystem);
        operation.getProducer().add(mockExternalItem("producer1", "producers", TypeExternalArtefactsEnum.AGENCY));
        operation.getProducer().add(mockExternalItem("producer22", "producers", TypeExternalArtefactsEnum.AGENCY));
        operation.getRegionalResponsible().add(mockExternalItem("regionalResponsible1", "regionalResponsibles", TypeExternalArtefactsEnum.AGENCY));
        operation.getRegionalResponsible().add(mockExternalItem("regionalResponsible22", "regionalResponsibles", TypeExternalArtefactsEnum.AGENCY));
        operation.getRegionalResponsible().add(mockExternalItem("regionalResponsible333", "regionalResponsibles", TypeExternalArtefactsEnum.AGENCY));
        operation.getRegionalContributor().add(mockExternalItem("regionalContributor1", "regionalContributors", TypeExternalArtefactsEnum.AGENCY));
        operation.getRegionalContributor().add(mockExternalItem("regionalContributor22", "regionalContributors", TypeExternalArtefactsEnum.AGENCY));
        operation.setInternalInventoryDate(new DateTime(2012, 12, 1, 13, 15, 14, 0));
        operation.setCurrentlyActive(Boolean.FALSE);
        operation.setStatus(StatusEnum.DESIGN);
        operation.setProcStatus(procStatus);
        operation.getPublisher().add(mockExternalItem("publisher1", "publishers", TypeExternalArtefactsEnum.AGENCY));
        operation.getPublisher().add(mockExternalItem("publisher22", "publishers", TypeExternalArtefactsEnum.AGENCY));
        operation.getPublisher().add(mockExternalItem("publisher333", "publishers", TypeExternalArtefactsEnum.AGENCY));
        operation.setRelPolUsAc(mockInternationalString("relPolUsAc", subCode));
        operation.setReleaseCalendar(Boolean.TRUE);
        operation.setReleaseCalendarAccess("http://releaseCalendarAccess1");
        operation.getUpdateFrequency().add(mockExternalItem("updateFrequency1", "updateFrequencies", TypeExternalArtefactsEnum.CODE));
        operation.getUpdateFrequency().add(mockExternalItem("updateFrequency22", "updateFrequencies", TypeExternalArtefactsEnum.CODE));
        operation.getUpdateFrequency().add(mockExternalItem("updateFrequency333", "updateFrequencies", TypeExternalArtefactsEnum.CODE));
        operation.getUpdateFrequency().add(mockExternalItem("updateFrequency4444", "updateFrequencies", TypeExternalArtefactsEnum.CODE));
        operation.setInventoryDate(new DateTime(2013, 2, 4, 13, 15, 14, 0));
        operation.setRevPolicy(mockInternationalString("revPolicy", subCode));
        operation.setRevPractice(mockInternationalString("revPractice", subCode));
        operation.setCommonMetadata(mockExternalItem("commonMetadata1", "nothing", TypeExternalArtefactsEnum.AGENCY));
        operation.setComment(mockInternationalString("comment", subCode));
        operation.setNotes(mockInternationalString("notes", subCode));

        return operation;
    }
    private static Family mockFamily(String subCode, ProcStatusEnum procStatus) {

        Family family = new Family();
        family.setCode("family" + subCode);
        family.setUrn(GeneratorUrnUtils.generateSiemacStatisticalFamilyUrn(family.getCode()));
        family.setTitle(mockInternationalString("family", subCode));
        family.setAcronym(mockInternationalString("acronym", subCode));
        family.setDescription(mockInternationalString("description", subCode));
        family.setInternalInventoryDate(new DateTime(2012, 12, 1, 13, 15, 14, 0));
        family.setProcStatus(procStatus);
        family.setInventoryDate(new DateTime(2013, 2, 4, 13, 15, 14, 0));
        family.addOperation(mockOperationRelatedEntity("1", ProcStatusEnum.PUBLISH_INTERNALLY));
        return family;
    }

    private static Instance mockInstance(String subCode, ProcStatusEnum procStatus) {

        Instance instance = new Instance();
        instance.setOperation(mockOperationRelatedEntity("1", ProcStatusEnum.PUBLISH_INTERNALLY));
        instance.setCode("instance" + subCode);
        instance.setUrn(GeneratorUrnUtils.generateSiemacStatisticalOperationInstanceUrn(instance.getOperation().getCode(), instance.getCode()));
        instance.setTitle(mockInternationalString("instance", subCode));
        instance.setAcronym(mockInternationalString("acronym", subCode));
        instance.setOrder(Integer.valueOf(2));
        instance.setDataDescription(mockInternationalString("dataDescription", subCode));
        instance.setStatisticalPopulation(mockInternationalString("statisticalPopulation", subCode));
        instance.addStatisticalUnit(mockExternalItem("statisticalUnit1", "statisticalUnits", TypeExternalArtefactsEnum.CONCEPT));
        instance.addStatisticalUnit(mockExternalItem("statisticalUnit22", "statisticalUnits", TypeExternalArtefactsEnum.CONCEPT));
        instance.setGeographicGranularity(mockExternalItem("geographicGranularity", "geographicGranularities", TypeExternalArtefactsEnum.CODELIST));
        instance.setGeographicComparability(mockInternationalString("geographicComparability", subCode));
        instance.setTemporalGranularity(mockExternalItem("temporalGranularity", "temporalGranularities", TypeExternalArtefactsEnum.CODELIST));
        instance.setTemporalComparability(mockInternationalString("temporalComparability", subCode));
        instance.setBasePeriod("2012");
        instance.addUnitMeasure(mockExternalItem("unitMeasure1", "unitMeasures", TypeExternalArtefactsEnum.CONCEPT));
        instance.setStatConcDef(mockInternationalString("statConcDef", subCode));
        instance.addStatConcDefList(mockExternalItem("statConcDefList1", "statConcDefLists", TypeExternalArtefactsEnum.CODELIST));
        instance.addStatConcDefList(mockExternalItem("statConcDefList22", "statConcDefLists", TypeExternalArtefactsEnum.CODELIST));
        instance.addStatConcDefList(mockExternalItem("statConcDefList333", "statConcDefLists", TypeExternalArtefactsEnum.CODELIST));
        instance.setClassSystem(mockInternationalString("classSystem", subCode));
        instance.addClassSystemList(mockExternalItem("classSystemList1", "classSystemLists", TypeExternalArtefactsEnum.CODELIST));
        instance.addClassSystemList(mockExternalItem("classSystemList22", "classSystemLists", TypeExternalArtefactsEnum.CODELIST));
        instance.setInstanceType(mockInstanceType("instanceType1"));
        instance.setInternalInventoryDate(new DateTime(2012, 12, 1, 13, 15, 14, 0));
        instance.setProcStatus(procStatus);
        instance.setDocMethod(mockInternationalString("docMethod", subCode));
        instance.setSurveySource(mockSurveySource("surveySource1"));
        instance.setCollMethod(mockCollMethod("collMethod1"));
        instance.addInformationSupplier(mockExternalItem("informationSupplier1", "informationSuppliers", TypeExternalArtefactsEnum.COMMON_METADATA));
        instance.addFreqColl(mockExternalItem("freqColl1", "freqColls", TypeExternalArtefactsEnum.CATEGORY_SCHEME));
        instance.addFreqColl(mockExternalItem("freqColl22", "freqColls", TypeExternalArtefactsEnum.CATEGORY_SCHEME));
        instance.setDataValidation(mockInternationalString("dataValidation", subCode));
        instance.setDataCompilation(mockInternationalString("dataCompilation", subCode));
        instance.setAdjustment(mockInternationalString("adjustment", subCode));
        instance.setCostBurden(mockInternationalString("costBurden", subCode));
        instance.addCost(mockCost("cost1"));
        instance.addCost(mockCost("cost22"));
        instance.addCost(mockCost("cost333"));
        instance.addCost(mockCost("cost4444"));
        instance.setInventoryDate(new DateTime(2013, 2, 4, 13, 15, 14, 0));
        instance.setQualityDoc(mockInternationalString("qualityDoc", subCode));
        instance.setQualityAssure(mockInternationalString("qualityAssure", subCode));
        instance.setQualityAssmnt(mockInternationalString("qualityAssmnt", subCode));
        instance.setUserNeeds(mockInternationalString("userNeeds", subCode));
        instance.setUserSat(mockInternationalString("userSat", subCode));
        instance.setCompleteness(mockInternationalString("completeness", subCode));
        instance.setTimeliness(mockInternationalString("timeliness", subCode));
        instance.setPunctuality(mockInternationalString("punctuality", subCode));
        instance.setAccuracyOverall(mockInternationalString("accuracyOverall", subCode));
        instance.setSamplingErr(mockInternationalString("samplingErr", subCode));
        instance.setNonsamplingErr(mockInternationalString("nonsamplingErr", subCode));
        instance.setCoherXDomain(mockInternationalString("coherXDom", subCode));
        instance.setCoherInternal(mockInternationalString("coherInternal", subCode));
        instance.setComment(mockInternationalString("comment", subCode));
        instance.setNotes(mockInternationalString("notes", subCode));

        return instance;
    }
    public static List<SurveyType> mockFindAllSurveyTypes() {
        List<SurveyType> surveyTypes = new ArrayList<SurveyType>();
        surveyTypes.add(mockSurveyType("surveyType1"));
        surveyTypes.add(mockSurveyType("surveyType2"));
        return surveyTypes;
    }

    public static List<OfficialityType> mockFindAllOfficialityTypes() {
        List<OfficialityType> officialityTypes = new ArrayList<OfficialityType>();
        officialityTypes.add(mockOfficialityType("officialityType1"));
        officialityTypes.add(mockOfficialityType("officialityType2"));
        officialityTypes.add(mockOfficialityType("officialityType3"));
        return officialityTypes;
    }

    public static List<InstanceType> mockFindAllInstanceTypes() {
        List<InstanceType> instanceTypes = new ArrayList<InstanceType>();
        instanceTypes.add(mockInstanceType("instanceType1"));
        instanceTypes.add(mockInstanceType("instanceType2"));
        return instanceTypes;
    }

    public static List<SurveySource> mockFindAllSurveySources() {
        List<SurveySource> surveySources = new ArrayList<SurveySource>();
        surveySources.add(mockSurveySource("surveySource1"));
        surveySources.add(mockSurveySource("surveySource2"));
        return surveySources;
    }

    public static List<CollMethod> mockFindAllCollMethods() {
        List<CollMethod> collMethods = new ArrayList<CollMethod>();
        collMethods.add(mockCollMethod("collMethod1"));
        collMethods.add(mockCollMethod("collMethod2"));
        return collMethods;
    }

    public static List<Cost> mockFindAllCosts() {
        List<Cost> costs = new ArrayList<Cost>();
        costs.add(mockCost("cost1"));
        costs.add(mockCost("cost2"));
        return costs;
    }

    private static InternationalString mockInternationalString(String metadata, String subCode) {
        String subTitle = subCode != null ? metadata + subCode : metadata;
        return mockInternationalString("es", subTitle + " en Español", "en", subTitle + " in English");
    }

    private static InternationalString mockInternationalString(String locale1, String label1, String locale2, String label2) {

        InternationalString internationalString = new InternationalString();

        LocalisedString internationalStringLocale1 = new LocalisedString();
        internationalStringLocale1.setLocale(locale1);
        internationalStringLocale1.setLabel(label1);
        internationalString.addText(internationalStringLocale1);

        LocalisedString internationalStringLocale2 = new LocalisedString();
        internationalStringLocale2.setLocale(locale2);
        internationalStringLocale2.setLabel(label2);
        internationalString.addText(internationalStringLocale2);

        return internationalString;
    }
}