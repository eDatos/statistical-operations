package org.siemac.metamac.statistical.operations.rest.internal.v1_0.utils;

import static org.junit.Assert.fail;

import java.math.BigInteger;

import org.joda.time.DateTime;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.rest.common.test.utils.MetamacRestMocks;
import org.siemac.metamac.rest.common.v1_0.domain.RelatedResource;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.core.enume.domain.StatusEnum;
import org.siemac.metamac.statistical.operations.rest.internal.RestInternalConstants;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.FamiliesNoPagedResult;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Family;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Instance;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.OperationsPagedResult;

public class StatisticalOperationsRestMocks {

    public static Operation mockOperation1(String baseApi) {
        return mockOperation(baseApi, "1", ProcStatusEnum.PUBLISH_INTERNALLY);
    }

    public static Operation mockOperation2(String baseApi) {
        return mockOperation(baseApi, "2", ProcStatusEnum.PUBLISH_INTERNALLY);
    }


    public static Operation mockOperation3(String baseApi) {
        return mockOperation(baseApi, "3", ProcStatusEnum.PUBLISH_EXTERNALLY);
    }
    

    public static Operation mockOperation4(String baseApi) {
        return mockOperation(baseApi, "4", ProcStatusEnum.PUBLISH_INTERNALLY);
    }
    

    public static Operation mockOperation5(String baseApi) {
        return mockOperation(baseApi, "5", ProcStatusEnum.PUBLISH_INTERNALLY);
    }
    

    public static Operation mockOperation6(String baseApi) {
        return mockOperation(baseApi, "6", ProcStatusEnum.PUBLISH_EXTERNALLY);
    }

    public static Family mockFamily1(String baseApi) {
        return mockFamily(baseApi, "1", ProcStatusEnum.PUBLISH_INTERNALLY);
    }

    public static Family mockFamily2(String baseApi) {
        return mockFamily(baseApi, "2", ProcStatusEnum.PUBLISH_EXTERNALLY);
    }

    public static Instance mockInstance1(String baseApi) {
        return mockInstance(baseApi, "1", "operation1", ProcStatusEnum.PUBLISH_INTERNALLY);
    }

    public static FamiliesNoPagedResult mockFamiliesNoPagedResultByOperation1(String baseApi) {
        FamiliesNoPagedResult familiesResult = new FamiliesNoPagedResult();
        familiesResult.setKind(RestInternalConstants.KIND_FAMILIES);
        familiesResult.setTotal(BigInteger.valueOf(2));
        familiesResult.getItems().add(mockFamily1RelatedResource(baseApi));
        familiesResult.getItems().add(mockFamily2RelatedResource(baseApi));
        return familiesResult;
    }
    
    public static OperationsPagedResult mockOperationsPagedResult(String baseApi, String limit, String offset) {
        OperationsPagedResult operationsPagedResult = new OperationsPagedResult();
        operationsPagedResult.setKind(RestInternalConstants.KIND_OPERATIONS);
        operationsPagedResult.setTotal(BigInteger.valueOf(9));
        if (limit == null && (offset == null || "0".equals(offset))) {
            operationsPagedResult.getItems().add(mockOperation1RelatedResource(baseApi));
            operationsPagedResult.getItems().add(mockOperation2RelatedResource(baseApi));
            operationsPagedResult.getItems().add(mockOperation3RelatedResource(baseApi));
            operationsPagedResult.getItems().add(mockOperation4RelatedResource(baseApi));
            operationsPagedResult.getItems().add(mockOperation5RelatedResource(baseApi));
            operationsPagedResult.getItems().add(mockOperation6RelatedResource(baseApi));
            operationsPagedResult.getItems().add(mockOperation7RelatedResource(baseApi));
            operationsPagedResult.getItems().add(mockOperation8RelatedResource(baseApi));
            operationsPagedResult.getItems().add(mockOperation9RelatedResource(baseApi));
            operationsPagedResult.setOffset(BigInteger.valueOf(0));
            operationsPagedResult.setLimit(BigInteger.valueOf(25));
            operationsPagedResult.setFirstLink(null);
            operationsPagedResult.setPreviousLink(null);
            operationsPagedResult.setNextLink(null);
            operationsPagedResult.setLastLink(null);
        } else if ("10000".equals(limit) && offset == null) {
            operationsPagedResult.getItems().add(mockOperation1RelatedResource(baseApi));
            operationsPagedResult.getItems().add(mockOperation2RelatedResource(baseApi));
            operationsPagedResult.getItems().add(mockOperation3RelatedResource(baseApi));
            operationsPagedResult.getItems().add(mockOperation4RelatedResource(baseApi));
            operationsPagedResult.getItems().add(mockOperation5RelatedResource(baseApi));
            operationsPagedResult.getItems().add(mockOperation6RelatedResource(baseApi));
            operationsPagedResult.getItems().add(mockOperation7RelatedResource(baseApi));
            operationsPagedResult.getItems().add(mockOperation8RelatedResource(baseApi));
            operationsPagedResult.getItems().add(mockOperation9RelatedResource(baseApi));
            operationsPagedResult.setOffset(BigInteger.valueOf(0));
            operationsPagedResult.setLimit(BigInteger.valueOf(1000));
            operationsPagedResult.setFirstLink(null);
            operationsPagedResult.setPreviousLink(null);
            operationsPagedResult.setNextLink(null);
            operationsPagedResult.setLastLink(null);
        } else if ("2".equals(limit) && "0".equals(offset)) {
            operationsPagedResult.getItems().add(mockOperation1RelatedResource(baseApi));
            operationsPagedResult.getItems().add(mockOperation2RelatedResource(baseApi));
            operationsPagedResult.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            operationsPagedResult.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            operationsPagedResult.setFirstLink(null);
            operationsPagedResult.setPreviousLink(null);
            operationsPagedResult.setNextLink(baseApi + "/operations?limit=2&offset=2");
            operationsPagedResult.setLastLink(baseApi + "/operations?limit=2&offset=8");
        } else if ("2".equals(limit) && "2".equals(offset)) {
            operationsPagedResult.getItems().add(mockOperation3RelatedResource(baseApi));
            operationsPagedResult.getItems().add(mockOperation4RelatedResource(baseApi));
            operationsPagedResult.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            operationsPagedResult.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            operationsPagedResult.setFirstLink(baseApi + "/operations?limit=2&offset=0");
            operationsPagedResult.setPreviousLink(baseApi + "/operations?limit=2&offset=0");
            operationsPagedResult.setNextLink(baseApi + "/operations?limit=2&offset=4");
            operationsPagedResult.setLastLink(baseApi + "/operations?limit=2&offset=8");
        } else if ("2".equals(limit) && "8".equals(offset)) {
            operationsPagedResult.getItems().add(mockOperation9RelatedResource(baseApi));
            operationsPagedResult.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            operationsPagedResult.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            operationsPagedResult.setFirstLink(baseApi + "/operations?limit=2&offset=0");
            operationsPagedResult.setPreviousLink(baseApi + "/operations?limit=2&offset=6");
            operationsPagedResult.setNextLink(null);
            operationsPagedResult.setLastLink(null);
        } else if ("2".equals(limit) && "9".equals(offset)) {
            // no results
            operationsPagedResult.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            operationsPagedResult.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            operationsPagedResult.setFirstLink(baseApi + "/operations?limit=2&offset=0");
            operationsPagedResult.setPreviousLink(null);
            operationsPagedResult.setNextLink(null);
            operationsPagedResult.setLastLink(null);
        } else {
            fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
        }
        return operationsPagedResult;
    }

    public static OperationsPagedResult mockOperationsPagedResultByFamily1(String baseApi, String limit, String offset) {
        OperationsPagedResult operationsPagedResult = new OperationsPagedResult();
        operationsPagedResult.setKind(RestInternalConstants.KIND_OPERATIONS);
        operationsPagedResult.setTotal(BigInteger.valueOf(6));
        if (limit == null && (offset == null || "0".equals(offset))) {
            operationsPagedResult.getItems().add(mockOperation1RelatedResource(baseApi));
            operationsPagedResult.getItems().add(mockOperation2RelatedResource(baseApi));
            operationsPagedResult.getItems().add(mockOperation3RelatedResource(baseApi));
            operationsPagedResult.getItems().add(mockOperation4RelatedResource(baseApi));
            operationsPagedResult.getItems().add(mockOperation5RelatedResource(baseApi));
            operationsPagedResult.getItems().add(mockOperation6RelatedResource(baseApi));
            operationsPagedResult.setOffset(BigInteger.valueOf(0));
            operationsPagedResult.setLimit(BigInteger.valueOf(25));
            operationsPagedResult.setFirstLink(null);
            operationsPagedResult.setPreviousLink(null);
            operationsPagedResult.setNextLink(null);
            operationsPagedResult.setLastLink(null);
        } else if ("10000".equals(limit) && offset == null) {
            operationsPagedResult.getItems().add(mockOperation1RelatedResource(baseApi));
            operationsPagedResult.getItems().add(mockOperation2RelatedResource(baseApi));
            operationsPagedResult.getItems().add(mockOperation3RelatedResource(baseApi));
            operationsPagedResult.getItems().add(mockOperation4RelatedResource(baseApi));
            operationsPagedResult.getItems().add(mockOperation5RelatedResource(baseApi));
            operationsPagedResult.getItems().add(mockOperation6RelatedResource(baseApi));
            operationsPagedResult.setOffset(BigInteger.valueOf(0));
            operationsPagedResult.setLimit(BigInteger.valueOf(1000));
            operationsPagedResult.setFirstLink(null);
            operationsPagedResult.setPreviousLink(null);
            operationsPagedResult.setNextLink(null);
            operationsPagedResult.setLastLink(null);
        } else if ("2".equals(limit) && "0".equals(offset)) {
            operationsPagedResult.getItems().add(mockOperation1RelatedResource(baseApi));
            operationsPagedResult.getItems().add(mockOperation2RelatedResource(baseApi));
            operationsPagedResult.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            operationsPagedResult.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            operationsPagedResult.setFirstLink(null);
            operationsPagedResult.setPreviousLink(null);
            operationsPagedResult.setNextLink(baseApi + "/families/family1/operations?limit=2&offset=2");
            operationsPagedResult.setLastLink(baseApi + "/families/family1/operations?limit=2&offset=4");
        } else if ("2".equals(limit) && "2".equals(offset)) {
            operationsPagedResult.getItems().add(mockOperation3RelatedResource(baseApi));
            operationsPagedResult.getItems().add(mockOperation4RelatedResource(baseApi));
            operationsPagedResult.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            operationsPagedResult.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            operationsPagedResult.setFirstLink(baseApi + "/families/family1/operations?limit=2&offset=0");
            operationsPagedResult.setPreviousLink(baseApi + "/families/family1/operations?limit=2&offset=0");
            operationsPagedResult.setNextLink(baseApi + "/families/family1/operations?limit=2&offset=4");
            operationsPagedResult.setLastLink(baseApi + "/families/family1/operations?limit=2&offset=4");
        } else if ("2".equals(limit) && "4".equals(offset)) {
            operationsPagedResult.getItems().add(mockOperation5RelatedResource(baseApi));
            operationsPagedResult.getItems().add(mockOperation6RelatedResource(baseApi));
            operationsPagedResult.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            operationsPagedResult.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            operationsPagedResult.setFirstLink(baseApi + "/families/family1/operations?limit=2&offset=0");
            operationsPagedResult.setPreviousLink(baseApi + "/families/family1/operations?limit=2&offset=2");
            operationsPagedResult.setNextLink(null);
            operationsPagedResult.setLastLink(null);
        } else if ("2".equals(limit) && "7".equals(offset)) {
            // no results
            operationsPagedResult.setOffset(BigInteger.valueOf(Integer.valueOf(offset).intValue()));
            operationsPagedResult.setLimit(BigInteger.valueOf(Integer.valueOf(limit).intValue()));
            operationsPagedResult.setFirstLink(baseApi + "/families/family1/operations?limit=2&offset=0");
            operationsPagedResult.setPreviousLink(null);
            operationsPagedResult.setNextLink(null);
            operationsPagedResult.setLastLink(null);
        } else {
            fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
        }
        return operationsPagedResult;
    }
    
    public static OperationsPagedResult mockOperationsPagedResultByFamily2(String baseApi, String limit, String offset) {
        OperationsPagedResult operationsPagedResult = new OperationsPagedResult();
        operationsPagedResult.setKind(RestInternalConstants.KIND_OPERATIONS);
        operationsPagedResult.setTotal(BigInteger.valueOf(4));
        if ("1".equals(limit) && "2".equals(offset)) {
            operationsPagedResult.getItems().add(mockOperation8RelatedResource(baseApi));
            operationsPagedResult.setOffset(BigInteger.valueOf(2));
            operationsPagedResult.setLimit(BigInteger.valueOf(1));
            operationsPagedResult.setFirstLink(baseApi + "/families/family2/operations?limit=1&offset=0");
            operationsPagedResult.setPreviousLink(baseApi + "/families/family2/operations?limit=1&offset=1");
            operationsPagedResult.setNextLink(baseApi + "/families/family2/operations?limit=1&offset=3");
            operationsPagedResult.setLastLink(baseApi + "/families/family2/operations?limit=1&offset=3");
        } else {
            fail("Limit or offset non supported. Limit = " + limit + ". Offset = " + offset);
        }
        return operationsPagedResult;
    }

    private static RelatedResource mockOperationRelatedResource(String subId, String baseApi) {
        return MetamacRestMocks.mockRelatedResource("operation" + subId, RestInternalConstants.KIND_OPERATION, baseApi + "/operations/operation" + subId, Boolean.TRUE, "operation");
    }

    private static RelatedResource mockFamilyRelatedResource(String subId, String baseApi) {
        return MetamacRestMocks.mockRelatedResource("family" + subId, RestInternalConstants.KIND_FAMILY, baseApi + "/families/family" + subId, Boolean.TRUE, "family");
    }

    private static RelatedResource mockInstanceRelatedResource(String operationId, String subId, String baseApi) {
        return MetamacRestMocks.mockRelatedResource("instance" + subId, RestInternalConstants.KIND_INSTANCE, baseApi + "/operations/" + operationId + "/instances/instance" + subId, Boolean.TRUE,
                "instance");
    }

    private static Operation mockOperation(String baseApi, String subCode, ProcStatusEnum procStatus) {

        Operation operation = new Operation();
        operation.setId("operation" + subCode);
        operation.setKind(RestInternalConstants.KIND_OPERATION);
        operation.setSelfLink(baseApi + "/operations/operation" + subCode);
        operation.setTitle(MetamacRestMocks.mockInternationalString("es", "Título operation operation" + subCode, "en", "Title operation operation" + subCode));
        operation.setAcronym(MetamacRestMocks.mockInternationalString("es", "Acrónimo " + subCode + " en español", "en", "Acronym " + subCode + " in English"));
        operation.getFamilies().add(mockFamilyRelatedResource("1", baseApi));
        operation.getFamilies().add(mockFamilyRelatedResource("2", baseApi));
        operation.setSubjectArea(MetamacRestMocks.mockRelatedResource("subjectArea1", TypeExternalArtefactsEnum.CATEGORY.name(), "http://subjectArea1", Boolean.FALSE, null));
        operation.getSecondarySubjectAreas().add(
                MetamacRestMocks.mockRelatedResource("secundarySubjectArea1", TypeExternalArtefactsEnum.CATEGORY.name(), "http://secundarySubjectArea1", Boolean.FALSE, null));
        operation.getSecondarySubjectAreas().add(
                MetamacRestMocks.mockRelatedResource("secundarySubjectArea22", TypeExternalArtefactsEnum.CATEGORY.name(), "http://secundarySubjectArea22", Boolean.FALSE, null));
        operation.getSecondarySubjectAreas().add(
                MetamacRestMocks.mockRelatedResource("secundarySubjectArea333", TypeExternalArtefactsEnum.CATEGORY.name(), "http://secundarySubjectArea333", Boolean.FALSE, null));
        operation.setObjective(MetamacRestMocks.mockInternationalString("es", "Objetivo " + subCode + " en español", "en", "Objective " + subCode + " in English"));
        operation.setDescription(MetamacRestMocks.mockInternationalString("es", "Descripción " + subCode + " en español", "en", "Description " + subCode + " in English"));
        operation.getInstances().add(mockInstanceRelatedResource(operation.getId(), "1", baseApi));
        operation.getInstances().add(mockInstanceRelatedResource(operation.getId(), "22", baseApi));
        operation.getInstances().add(mockInstanceRelatedResource(operation.getId(), "333", baseApi));
        operation.getInstances().add(mockInstanceRelatedResource(operation.getId(), "4444", baseApi));
        operation.setSurveyType(MetamacRestMocks.mockRelatedResource("surveyIdentifier", null, null, Boolean.TRUE, "survey"));
        operation.setOfficialityType(MetamacRestMocks.mockRelatedResource("officialityTypeIdentifier", null, null, Boolean.TRUE, "officialityType"));
        operation.setIndicatorSystem(Boolean.TRUE);
        operation.getProducers().add(MetamacRestMocks.mockRelatedResource("producer1", TypeExternalArtefactsEnum.AGENCY.name(), "http://producer1", Boolean.FALSE, null));
        operation.getProducers().add(MetamacRestMocks.mockRelatedResource("producer22", TypeExternalArtefactsEnum.AGENCY.name(), "http://producer22", Boolean.FALSE, null));
        operation.getRegionalResponsibles().add(
                MetamacRestMocks.mockRelatedResource("regionalResponsible1", TypeExternalArtefactsEnum.AGENCY.name(), "http://regionalResponsible1", Boolean.FALSE, null));
        operation.getRegionalResponsibles().add(
                MetamacRestMocks.mockRelatedResource("regionalResponsible22", TypeExternalArtefactsEnum.AGENCY.name(), "http://regionalResponsible22", Boolean.FALSE, null));
        operation.getRegionalResponsibles().add(
                MetamacRestMocks.mockRelatedResource("regionalResponsible333", TypeExternalArtefactsEnum.AGENCY.name(), "http://regionalResponsible333", Boolean.FALSE, null));
        operation.getRegionalContributors().add(
                MetamacRestMocks.mockRelatedResource("regionalContributor1", TypeExternalArtefactsEnum.AGENCY.name(), "http://regionalContributor1", Boolean.FALSE, null));
        operation.getRegionalContributors().add(
                MetamacRestMocks.mockRelatedResource("regionalContributor22", TypeExternalArtefactsEnum.AGENCY.name(), "http://regionalContributor22", Boolean.FALSE, null));
        operation.setInternalInventoryDate(new DateTime(2012, 12, 1, 13, 15, 14, 0).toDate());
        operation.setCurrentlyActive(Boolean.FALSE);
        operation.setStatus(StatusEnum.DESIGN.name());
        operation.setProcStatus(procStatus.name());
        operation.getPublishers().add(MetamacRestMocks.mockRelatedResource("publisher1", TypeExternalArtefactsEnum.AGENCY.name(), "http://publisher1", Boolean.FALSE, null));
        operation.getPublishers().add(MetamacRestMocks.mockRelatedResource("publisher22", TypeExternalArtefactsEnum.AGENCY.name(), "http://publisher22", Boolean.FALSE, null));
        operation.getPublishers().add(MetamacRestMocks.mockRelatedResource("publisher333", TypeExternalArtefactsEnum.AGENCY.name(), "http://publisher333", Boolean.FALSE, null));
        operation.setRelPolUsAc(MetamacRestMocks.mockInternationalString("es", "RelPolUsAc " + subCode + " en español", "en", "RelPolUsAc " + subCode + " in English"));
        operation.setReleaseCalendar(Boolean.TRUE);
        operation.setReleaseCalendarAccess("http://releaseCalendarAccess1");
        operation.getUpdateFrequencies().add(MetamacRestMocks.mockRelatedResource("updateFrequency1", TypeExternalArtefactsEnum.CODE.name(), "http://updateFrequency1", Boolean.FALSE, null));
        operation.getUpdateFrequencies().add(MetamacRestMocks.mockRelatedResource("updateFrequency22", TypeExternalArtefactsEnum.CODE.name(), "http://updateFrequency22", Boolean.FALSE, null));
        operation.getUpdateFrequencies().add(MetamacRestMocks.mockRelatedResource("updateFrequency333", TypeExternalArtefactsEnum.CODE.name(), "http://updateFrequency333", Boolean.FALSE, null));
        operation.getUpdateFrequencies().add(MetamacRestMocks.mockRelatedResource("updateFrequency4444", TypeExternalArtefactsEnum.CODE.name(), "http://updateFrequency4444", Boolean.FALSE, null));
        operation.setCurrentInternalInstance(mockInstanceRelatedResource(operation.getId(), "22", baseApi));
        operation.setCurrentInstance(mockInstanceRelatedResource(operation.getId(), "333", baseApi));
        operation.setInventoryDate(new DateTime(2013, 2, 4, 13, 15, 14, 0).toDate());
        operation.setRevPolicy(MetamacRestMocks.mockInternationalString("es", "RevPolicy " + subCode + " en español", "en", "RevPolicy " + subCode + " in English"));
        operation.setRevPractice(MetamacRestMocks.mockInternationalString("es", "RevPractice " + subCode + " en español", "en", "RevPractice " + subCode + " in English"));
        // TODO CONTACTS, LEGAL_ACTS, DATA_SHARING, CONFIDENTIALITY_POLICY, CONFIDENTIALITY_DATA_TREATMENT. No están en OperationBase
        operation.setComment(MetamacRestMocks.mockInternationalString("es", "Comentarios " + subCode + " en español", "en", "Comments " + subCode + " in English"));
        operation.setNotes(MetamacRestMocks.mockInternationalString("es", "Notas " + subCode + " en español", "en", "Notes " + subCode + " in English"));
        operation.setParent(MetamacRestMocks.mockRelatedResource(null, RestInternalConstants.KIND_OPERATIONS, baseApi + "/operations", Boolean.FALSE, null));
        operation.getchildren().add(MetamacRestMocks.mockRelatedResource(null, RestInternalConstants.KIND_FAMILIES, baseApi + "/operations/operation" + subCode + "/families", Boolean.FALSE, null));
        operation.getchildren().add(MetamacRestMocks.mockRelatedResource(null, RestInternalConstants.KIND_INSTANCES, baseApi + "/operations/operation" + subCode + "/instances", Boolean.FALSE, null));
        return operation;
    }

    private static Family mockFamily(String baseApi, String subCode, ProcStatusEnum procStatus) {

        Family family = new Family();
        family.setId("family" + subCode);
        family.setKind(RestInternalConstants.KIND_FAMILY);
        family.setSelfLink(baseApi + "/families/family" + subCode);
        family.setTitle(MetamacRestMocks.mockInternationalString("es", "Título family family" + subCode, "en", "Title family family" + subCode));
        family.setAcronym(MetamacRestMocks.mockInternationalString("es", "Acrónimo " + subCode + " en español", "en", "Acronym " + subCode + " in English"));
        family.setDescription(MetamacRestMocks.mockInternationalString("es", "Descripción " + subCode + " en español", "en", "Description " + subCode + " in English"));
        family.setInternalInventoryDate(new DateTime(2012, 12, 1, 13, 15, 14, 0).toDate());
        family.setProcStatus(procStatus.name());
        family.setInventoryDate(new DateTime(2013, 2, 4, 13, 15, 14, 0).toDate());
        family.setParent(MetamacRestMocks.mockRelatedResource(null, RestInternalConstants.KIND_FAMILIES, baseApi + "/families", Boolean.FALSE, null));
        family.getchildren().add(MetamacRestMocks.mockRelatedResource(null, RestInternalConstants.KIND_OPERATIONS, baseApi + "/families/family" + subCode + "/operations", Boolean.FALSE, null));
        return family;
    }

    private static Instance mockInstance(String baseApi, String subCode, String operation, ProcStatusEnum procStatus) {

        Instance instance = new Instance();
        instance.setId("instance" + subCode);
        instance.setKind(RestInternalConstants.KIND_INSTANCE);
        instance.setSelfLink(baseApi + "/operations/" + operation + "/instances/instance" + subCode);
        instance.setTitle(MetamacRestMocks.mockInternationalString("es", "Título instance instance" + subCode, "en", "Title instance instance" + subCode));
        instance.setAcronym(MetamacRestMocks.mockInternationalString("es", "Acrónimo " + subCode + " en español", "en", "Acronym " + subCode + " in English"));
        instance.setSurvey(MetamacRestMocks.mockRelatedResource(operation, RestInternalConstants.KIND_OPERATION, baseApi + "/operations/" + operation, Boolean.TRUE, "operation"));
        instance.setPredecessor(mockInstanceRelatedResource(operation, "333", baseApi));
        instance.setSuccessor(mockInstanceRelatedResource(operation, "22", baseApi));
        instance.setDataDescription(MetamacRestMocks.mockInternationalString("es", "Descripción de Datos " + subCode + " en español", "en", "DataDescription " + subCode + " in English"));
        instance.setStatisticalPopulation(MetamacRestMocks.mockInternationalString("es", "Carga de Estadísticas " + subCode + " en español", "en", "StatisticalPopulation " + subCode + " in English"));
        instance.getStatisticalUnits().add(MetamacRestMocks.mockRelatedResource("statisticalUnit1", TypeExternalArtefactsEnum.DATASTRUCTURE.name(), "http://statisticalUnit1", Boolean.FALSE, null));
        instance.getStatisticalUnits().add(MetamacRestMocks.mockRelatedResource("statisticalUnit22", TypeExternalArtefactsEnum.DATASTRUCTURE.name(), "http://statisticalUnit22", Boolean.FALSE, null));
        instance.setGeographicGranularity(MetamacRestMocks.mockRelatedResource("geographicGranularity", TypeExternalArtefactsEnum.CODELIST.name(), "http://geographicGranularity", Boolean.FALSE, null));
        instance.setGeographicComparability(MetamacRestMocks.mockInternationalString("es", "Comparando Geográficos " + subCode + " en español", "en", "geographicComparability " + subCode
                + " in English"));
        instance.setTemporalGranularity(MetamacRestMocks.mockRelatedResource("temporalGranularity", TypeExternalArtefactsEnum.CODELIST.name(), "http://temporalGranularity", Boolean.FALSE, null));
        instance.setTemporalComparability(MetamacRestMocks.mockInternationalString("es", "Comparando Temporal " + subCode + " en español", "en", "temporalComparability " + subCode + " in English"));
        instance.setBasePeriod("2012");
        instance.getUnitMeasures().add(MetamacRestMocks.mockRelatedResource("unitMeasure1", TypeExternalArtefactsEnum.CONCEPT.name(), "http://unitMeasure1", Boolean.FALSE, null));
        instance.setStatConcDef(MetamacRestMocks.mockInternationalString("es", "StatConcDef " + subCode + " en español", "en", "StatConcDef " + subCode + " in English"));
        instance.getStatConcDefLists().add(MetamacRestMocks.mockRelatedResource("statConcDefList1", TypeExternalArtefactsEnum.CODELIST.name(), "http://statConcDefList1", Boolean.FALSE, null));
        instance.getStatConcDefLists().add(MetamacRestMocks.mockRelatedResource("statConcDefList22", TypeExternalArtefactsEnum.CODELIST.name(), "http://statConcDefList22", Boolean.FALSE, null));
        instance.getStatConcDefLists().add(MetamacRestMocks.mockRelatedResource("statConcDefList333", TypeExternalArtefactsEnum.CODELIST.name(), "http://statConcDefList333", Boolean.FALSE, null));
        instance.setClassSystem(MetamacRestMocks.mockInternationalString("es", "ClassSystem " + subCode + " en español", "en", "ClassSystem " + subCode + " in English"));
        instance.getClassSystemLists().add(MetamacRestMocks.mockRelatedResource("statConcDefList1", TypeExternalArtefactsEnum.CODELIST.name(), "http://statConcDefList1", Boolean.FALSE, null));
        instance.getClassSystemLists().add(MetamacRestMocks.mockRelatedResource("statConcDefList22", TypeExternalArtefactsEnum.CODELIST.name(), "http://statConcDefList22", Boolean.FALSE, null));
        instance.setInstanceType(MetamacRestMocks.mockRelatedResource("instanceType1", null, null, Boolean.TRUE, "instanceType"));
        instance.setInternalInventoryDate(new DateTime(2012, 12, 1, 13, 15, 14, 0).toDate());
        instance.setProcStatus(procStatus.name());
        instance.setDocMethod(MetamacRestMocks.mockInternationalString("es", "DocMethod " + subCode + " en español", "en", "DocMethod " + subCode + " in English"));
        instance.setSurveySource(MetamacRestMocks.mockRelatedResource("surveySource1", null, null, Boolean.TRUE, "surveySource"));
        instance.setCollMethod(MetamacRestMocks.mockRelatedResource("collMethod1", null, null, Boolean.TRUE, "collMethod"));
        instance.getInformationSuppliers().add(
                MetamacRestMocks.mockRelatedResource("informationSupplier1", TypeExternalArtefactsEnum.COMMON_METADATA.name(), "http://informationSupplier1", Boolean.FALSE, null));
        instance.getFreqColls().add(MetamacRestMocks.mockRelatedResource("freqColl1", TypeExternalArtefactsEnum.CATEGORY_SCHEME.name(), "http://freqColl1", Boolean.FALSE, null));
        instance.getFreqColls().add(MetamacRestMocks.mockRelatedResource("freqColl22", TypeExternalArtefactsEnum.CATEGORY_SCHEME.name(), "http://freqColl22", Boolean.FALSE, null));
        instance.setDataValidation(MetamacRestMocks.mockInternationalString("es", "DataValidation " + subCode + " en español", "en", "DataValidation " + subCode + " in English"));
        instance.setDataCompilation(MetamacRestMocks.mockInternationalString("es", "DataCompilation " + subCode + " en español", "en", "DataCompilation " + subCode + " in English"));
        instance.setAdjustment(MetamacRestMocks.mockInternationalString("es", "Adjustment " + subCode + " en español", "en", "Adjustment " + subCode + " in English"));
        instance.setCostBurden(MetamacRestMocks.mockInternationalString("es", "CostBurden " + subCode + " en español", "en", "CostBurden " + subCode + " in English"));
        instance.getCosts().add(MetamacRestMocks.mockRelatedResource("cost1", null, null, Boolean.TRUE, "cost"));
        instance.getCosts().add(MetamacRestMocks.mockRelatedResource("cost22", null, null, Boolean.TRUE, "cost"));
        instance.getCosts().add(MetamacRestMocks.mockRelatedResource("cost333", null, null, Boolean.TRUE, "cost"));
        instance.getCosts().add(MetamacRestMocks.mockRelatedResource("cost4444", null, null, Boolean.TRUE, "cost"));
        instance.setInventoryDate(new DateTime(2013, 2, 4, 13, 15, 14, 0).toDate());
        instance.setQualityDoc(MetamacRestMocks.mockInternationalString("es", "QualityDoc " + subCode + " en español", "en", "QualityDoc " + subCode + " in English"));
        instance.setQualityAssure(MetamacRestMocks.mockInternationalString("es", "QualityAssure " + subCode + " en español", "en", "QualityAssure " + subCode + " in English"));
        instance.setQualityAssmnt(MetamacRestMocks.mockInternationalString("es", "QualityAssmnt " + subCode + " en español", "en", "QualityAssmnt " + subCode + " in English"));
        instance.setUserNeeds(MetamacRestMocks.mockInternationalString("es", "UserNeeds " + subCode + " en español", "en", "UserNeeds " + subCode + " in English"));
        instance.setUserSat(MetamacRestMocks.mockInternationalString("es", "UserSat " + subCode + " en español", "en", "UserSat " + subCode + " in English"));
        instance.setCompleteness(MetamacRestMocks.mockInternationalString("es", "Completeness " + subCode + " en español", "en", "Completeness " + subCode + " in English"));
        instance.setTimeliness(MetamacRestMocks.mockInternationalString("es", "Timeliness " + subCode + " en español", "en", "Timeliness " + subCode + " in English"));
        instance.setPunctuality(MetamacRestMocks.mockInternationalString("es", "Punctuality " + subCode + " en español", "en", "Punctuality " + subCode + " in English"));
        instance.setAccuracyOverall(MetamacRestMocks.mockInternationalString("es", "AccuracyOverall " + subCode + " en español", "en", "AccuracyOverall " + subCode + " in English"));
        instance.setSamplingErr(MetamacRestMocks.mockInternationalString("es", "SamplingErr " + subCode + " en español", "en", "SamplingErr " + subCode + " in English"));
        instance.setNonsamplingErr(MetamacRestMocks.mockInternationalString("es", "NonsamplingErr " + subCode + " en español", "en", "NonsamplingErr " + subCode + " in English"));
        instance.setCoherXDom(MetamacRestMocks.mockInternationalString("es", "CoherXDom " + subCode + " en español", "en", "CoherXDom " + subCode + " in English"));
        instance.setCoherInternal(MetamacRestMocks.mockInternationalString("es", "CoherInternal " + subCode + " en español", "en", "CoherInternal " + subCode + " in English"));
        instance.setComment(MetamacRestMocks.mockInternationalString("es", "Comentarios " + subCode + " en español", "en", "Comments " + subCode + " in English"));
        instance.setNotes(MetamacRestMocks.mockInternationalString("es", "Notas " + subCode + " en español", "en", "Notes " + subCode + " in English"));
        instance.setParent(MetamacRestMocks.mockRelatedResource(null, RestInternalConstants.KIND_OPERATION, baseApi + "/operations/" + operation, Boolean.FALSE, null));
        instance.getchildren(); // no children
        return instance;
    }
    
    
    private static RelatedResource mockOperation1RelatedResource(String baseApi) {
        return mockOperationRelatedResource("1", baseApi);
    }

    private static RelatedResource mockOperation2RelatedResource(String baseApi) {
        return mockOperationRelatedResource("2", baseApi);
    }
    
    private static RelatedResource mockOperation3RelatedResource(String baseApi) {
        return mockOperationRelatedResource("3", baseApi);
    }
    
    private static RelatedResource mockOperation4RelatedResource(String baseApi) {
        return mockOperationRelatedResource("4", baseApi);
    }
    
    private static RelatedResource mockOperation5RelatedResource(String baseApi) {
        return mockOperationRelatedResource("5", baseApi);
    }
    
    private static RelatedResource mockOperation6RelatedResource(String baseApi) {
        return mockOperationRelatedResource("6", baseApi);
    }
    
    private static RelatedResource mockOperation7RelatedResource(String baseApi) {
        return mockOperationRelatedResource("7", baseApi);
    }
    
    private static RelatedResource mockOperation8RelatedResource(String baseApi) {
        return mockOperationRelatedResource("8", baseApi);
    }
    
    private static RelatedResource mockOperation9RelatedResource(String baseApi) {
        return mockOperationRelatedResource("9", baseApi);
    }
    
    private static RelatedResource mockFamily1RelatedResource(String baseApi) {
        return mockFamilyRelatedResource("1", baseApi);
    }
    
    private static RelatedResource mockFamily2RelatedResource(String baseApi) {
        return mockFamilyRelatedResource("2", baseApi);
    }
}