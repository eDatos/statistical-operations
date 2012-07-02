package org.siemac.metamac.statistical.operations.rest.internal.v1_0.utils;

import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.List;

import org.fornax.cartridges.sculptor.framework.domain.PagedResult;
import org.joda.time.DateTime;
import org.siemac.metamac.core.common.bt.domain.ExternalItemBt;
import org.siemac.metamac.core.common.ent.domain.InternationalString;
import org.siemac.metamac.core.common.ent.domain.LocalisedString;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.core.common.vo.domain.ExternalItem;
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

public class StatisticalOperationsCoreMocks {

    public static Operation mockOperation1() {
        return mockOperation("1", ProcStatusEnum.PUBLISH_INTERNALLY, mockFamily1RelatedEntity(), mockFamily2RelatedEntity());
    }

    public static Operation mockOperation2() {
        return mockOperation("2", ProcStatusEnum.PUBLISH_INTERNALLY, mockFamily1RelatedEntity());
    }

    public static Operation mockOperation3() {
        return mockOperation("3", ProcStatusEnum.PUBLISH_EXTERNALLY, mockFamily1RelatedEntity());
    }

    public static Operation mockOperation4() {
        return mockOperation("4", ProcStatusEnum.PUBLISH_INTERNALLY, mockFamily1RelatedEntity());
    }

    public static Operation mockOperation5() {
        return mockOperation("5", ProcStatusEnum.PUBLISH_INTERNALLY, mockFamily1RelatedEntity());
    }

    public static Operation mockOperation6() {
        return mockOperation("6", ProcStatusEnum.PUBLISH_EXTERNALLY, mockFamily1RelatedEntity());
    }

    public static Operation mockOperation7() {
        return mockOperation("7", ProcStatusEnum.PUBLISH_EXTERNALLY, mockFamily2RelatedEntity());
    }

    public static Operation mockOperation8() {
        return mockOperation("8", ProcStatusEnum.PUBLISH_EXTERNALLY, mockFamily2RelatedEntity());
    }

    public static Operation mockOperation9() {
        return mockOperation("9", ProcStatusEnum.PUBLISH_EXTERNALLY, mockFamily2RelatedEntity());
    }

    public static Family mockFamily1() {
        return mockFamily("1", ProcStatusEnum.PUBLISH_INTERNALLY);
    }

    public static Family mockFamily2() {
        return mockFamily("2", ProcStatusEnum.PUBLISH_EXTERNALLY);
    }

    public static Instance mockInstance1() {
        return mockInstance("1", ProcStatusEnum.PUBLISH_INTERNALLY);
    }

    public static PagedResult<Family> mockFamiliesOperation1() {

        List<Family> families = new ArrayList<Family>();
        families.add(mockFamily1());
        families.add(mockFamily2());

        int startRow = 0;
        int rowCount = 2;
        int pageSize = 2;
        int totalRows = 2;

        return new PagedResult<Family>(families, startRow, rowCount, pageSize, totalRows, -1);
    }

    public static PagedResult<Operation> mockOperations(String limit, String offset) {

        List<Operation> operations = new ArrayList<Operation>();
        int total = 9;
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

    public static PagedResult<Operation> mockOperationsFamily1(String limit, String offset) {

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
    
    public static PagedResult<Operation> mockOperationsFamily2(String limit, String offset) {

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

    /**
     * Operation with basic attributes. Do not use mockInstance to avoid cyclic method calls
     */
    private static Operation mockOperationRelatedEntity(String subCode, ProcStatusEnum procStatus) {
        Operation operation = new Operation();
        operation.setCode("operation" + subCode);
        operation.setTitle(mockInternationalString("es", "Título operation operation" + subCode, "en", "Title operation operation" + subCode));
        operation.setProcStatus(procStatus);
        operation.addInstance(mockInstanceRelatedEntity("4444", ProcStatusEnum.DRAFT, Integer.valueOf(0)));
        operation.addInstance(mockInstanceRelatedEntity("22", ProcStatusEnum.PUBLISH_INTERNALLY, Integer.valueOf(3)));
        operation.addInstance(mockInstanceRelatedEntity("1", ProcStatusEnum.PUBLISH_INTERNALLY, Integer.valueOf(2)));
        operation.addInstance(mockInstanceRelatedEntity("333", ProcStatusEnum.PUBLISH_EXTERNALLY, Integer.valueOf(1)));
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
        family.setTitle(mockInternationalString("es", "Título family family" + subCode, "en", "Title family family" + subCode));
        family.setProcStatus(procStatus);
        return family;
    }

    /**
     * Instance with basic attributes. Do not use mockInstance to avoid cyclic method calls
     */
    private static Instance mockInstanceRelatedEntity(String subCode, ProcStatusEnum procStatus, Integer order) {
        Instance instance = new Instance();
        instance.setCode("instance" + subCode);
        instance.setTitle(mockInternationalString("es", "Título instance instance" + subCode, "en", "Title instance instance" + subCode));
        instance.setProcStatus(procStatus);
        instance.setOrder(order);
        return instance;
    }

    private static SurveyType mockSurveyType(String code) {
        SurveyType surveyType = new SurveyType();
        surveyType.setDescription(mockInternationalString("es", "Título survey " + code, "en", "Title survey " + code));
        surveyType.setIdentifier(code);
        return surveyType;
    }

    private static OfficialityType mockOfficialityType(String code) {
        OfficialityType officialityType = new OfficialityType();
        officialityType.setDescription(mockInternationalString("es", "Título officialityType " + code, "en", "Title officialityType " + code));
        officialityType.setIdentifier(code);
        return officialityType;
    }

    private static InstanceType mockInstanceType(String code) {
        InstanceType instanceType = new InstanceType();
        instanceType.setDescription(mockInternationalString("es", "Título instanceType " + code, "en", "Title instanceType " + code));
        instanceType.setIdentifier(code);
        return instanceType;
    }

    private static SurveySource mockSurveySource(String code) {
        SurveySource surveySource = new SurveySource();
        surveySource.setDescription(mockInternationalString("es", "Título surveySource " + code, "en", "Title surveySource " + code));
        surveySource.setIdentifier(code);
        return surveySource;
    }

    private static CollMethod mockCollMethod(String code) {
        CollMethod collMethod = new CollMethod();
        collMethod.setDescription(mockInternationalString("es", "Título collMethod " + code, "en", "Title collMethod " + code));
        collMethod.setIdentifier(code);
        return collMethod;
    }

    private static Cost mockCost(String code) {
        Cost cost = new Cost();
        cost.setDescription(mockInternationalString("es", "Título cost " + code, "en", "Title cost " + code));
        cost.setIdentifier(code);
        return cost;
    }

    private static ExternalItemBt mockExternalItemBt(String code, TypeExternalArtefactsEnum type, String uri) {
        return new ExternalItemBt(uri, code, type);
    }

    private static ExternalItem mockExternalItem(String code, TypeExternalArtefactsEnum type, String uri) {
        return new ExternalItem(new ExternalItemBt(uri, code, type));
    }

    private static Operation mockOperation(String subCode, ProcStatusEnum procStatus, Family... families) {

        Operation operation = new Operation();
        operation.setCode("operation" + subCode);
        operation.setTitle(mockInternationalString("es", "Título operation operation" + subCode, "en", "Title operation operation" + subCode));
        operation.setAcronym(mockInternationalString("es", "Acrónimo " + subCode + " en español", "en", "Acronym " + subCode + " in English"));
        if (families != null) {
            for (int i = 0; i < families.length; i++) {
                Family family = families[i];
                operation.addFamily(family);
            }
        }
        operation.setSubjectArea(mockExternalItemBt("subjectArea1", TypeExternalArtefactsEnum.CATEGORY, "http://subjectArea1"));
        operation.getSecondarySubjectAreas().add(mockExternalItem("secundarySubjectArea1", TypeExternalArtefactsEnum.CATEGORY, "http://secundarySubjectArea1"));
        operation.getSecondarySubjectAreas().add(mockExternalItem("secundarySubjectArea22", TypeExternalArtefactsEnum.CATEGORY, "http://secundarySubjectArea22"));
        operation.getSecondarySubjectAreas().add(mockExternalItem("secundarySubjectArea333", TypeExternalArtefactsEnum.CATEGORY, "http://secundarySubjectArea333"));
        operation.setObjective(mockInternationalString("es", "Objetivo " + subCode + " en español", "en", "Objective " + subCode + " in English"));
        operation.setDescription(mockInternationalString("es", "Descripción " + subCode + " en español", "en", "Description " + subCode + " in English"));
        operation.addInstance(mockInstanceRelatedEntity("4444", ProcStatusEnum.DRAFT, Integer.valueOf(0)));
        operation.addInstance(mockInstanceRelatedEntity("22", ProcStatusEnum.PUBLISH_INTERNALLY, Integer.valueOf(3)));
        operation.addInstance(mockInstanceRelatedEntity("1", ProcStatusEnum.PUBLISH_INTERNALLY, Integer.valueOf(2)));
        operation.addInstance(mockInstanceRelatedEntity("333", ProcStatusEnum.PUBLISH_EXTERNALLY, Integer.valueOf(1)));
        operation.setSurveyType(mockSurveyType("surveyIdentifier"));
        operation.setOfficialityType(mockOfficialityType("officialityTypeIdentifier"));
        operation.setIndicatorSystem(Boolean.TRUE);
        operation.getProducer().add(mockExternalItem("producer1", TypeExternalArtefactsEnum.AGENCY, "http://producer1"));
        operation.getProducer().add(mockExternalItem("producer22", TypeExternalArtefactsEnum.AGENCY, "http://producer22"));
        operation.getRegionalResponsible().add(mockExternalItem("regionalResponsible1", TypeExternalArtefactsEnum.AGENCY, "http://regionalResponsible1"));
        operation.getRegionalResponsible().add(mockExternalItem("regionalResponsible22", TypeExternalArtefactsEnum.AGENCY, "http://regionalResponsible22"));
        operation.getRegionalResponsible().add(mockExternalItem("regionalResponsible333", TypeExternalArtefactsEnum.AGENCY, "http://regionalResponsible333"));
        operation.getRegionalContributor().add(mockExternalItem("regionalContributor1", TypeExternalArtefactsEnum.AGENCY, "http://regionalContributor1"));
        operation.getRegionalContributor().add(mockExternalItem("regionalContributor22", TypeExternalArtefactsEnum.AGENCY, "http://regionalContributor22"));
        operation.setInternalInventoryDate(new DateTime(2012, 12, 1, 13, 15, 14, 0));
        operation.setCurrentlyActive(Boolean.FALSE);
        operation.setStatus(StatusEnum.DESIGN);
        operation.setProcStatus(procStatus);
        operation.getPublisher().add(mockExternalItem("publisher1", TypeExternalArtefactsEnum.AGENCY, "http://publisher1"));
        operation.getPublisher().add(mockExternalItem("publisher22", TypeExternalArtefactsEnum.AGENCY, "http://publisher22"));
        operation.getPublisher().add(mockExternalItem("publisher333", TypeExternalArtefactsEnum.AGENCY, "http://publisher333"));
        operation.setRelPolUsAc(mockInternationalString("es", "RelPolUsAc " + subCode + " en español", "en", "RelPolUsAc " + subCode + " in English"));
        operation.setReleaseCalendar(Boolean.TRUE);
        operation.setReleaseCalendarAccess("http://releaseCalendarAccess1");
        operation.getUpdateFrequency().add(mockExternalItem("updateFrequency1", TypeExternalArtefactsEnum.CODE, "http://updateFrequency1"));
        operation.getUpdateFrequency().add(mockExternalItem("updateFrequency22", TypeExternalArtefactsEnum.CODE, "http://updateFrequency22"));
        operation.getUpdateFrequency().add(mockExternalItem("updateFrequency333", TypeExternalArtefactsEnum.CODE, "http://updateFrequency333"));
        operation.getUpdateFrequency().add(mockExternalItem("updateFrequency4444", TypeExternalArtefactsEnum.CODE, "http://updateFrequency4444"));
        operation.setInventoryDate(new DateTime(2013, 2, 4, 13, 15, 14, 0));
        operation.setRevPolicy(mockInternationalString("es", "RevPolicy " + subCode + " en español", "en", "RevPolicy " + subCode + " in English"));
        operation.setRevPractice(mockInternationalString("es", "RevPractice " + subCode + " en español", "en", "RevPractice " + subCode + " in English"));
        // TODO CONTACTS, LEGAL_ACTS, DATA_SHARING, CONFIDENTIALITY_POLICY, CONFIDENTIALITY_DATA_TREATMENT. No están en OperationBase
        operation.setComment(mockInternationalString("es", "Comentarios " + subCode + " en español", "en", "Comments " + subCode + " in English"));
        operation.setNotes(mockInternationalString("es", "Notas " + subCode + " en español", "en", "Notes " + subCode + " in English"));

        return operation;
    }

    public static Family mockFamily(String subCode, ProcStatusEnum procStatus) {

        Family family = new Family();
        family.setCode("family" + subCode);
        family.setTitle(mockInternationalString("es", "Título family family" + subCode, "en", "Title family family" + subCode));
        family.setAcronym(mockInternationalString("es", "Acrónimo " + subCode + " en español", "en", "Acronym " + subCode + " in English"));
        family.setDescription(mockInternationalString("es", "Descripción " + subCode + " en español", "en", "Description " + subCode + " in English"));
        family.setInternalInventoryDate(new DateTime(2012, 12, 1, 13, 15, 14, 0));
        family.setProcStatus(procStatus);
        family.setInventoryDate(new DateTime(2013, 2, 4, 13, 15, 14, 0));
        family.addOperation(mockOperationRelatedEntity("1", ProcStatusEnum.PUBLISH_INTERNALLY));
        return family;
    }

    public static Instance mockInstance(String subCode, ProcStatusEnum procStatus) {

        Instance instance = new Instance();
        instance.setCode("instance" + subCode);
        instance.setTitle(mockInternationalString("es", "Título instance instance" + subCode, "en", "Title instance instance" + subCode));
        instance.setAcronym(mockInternationalString("es", "Acrónimo " + subCode + " en español", "en", "Acronym " + subCode + " in English"));
        instance.setOperation(mockOperationRelatedEntity("1", ProcStatusEnum.PUBLISH_INTERNALLY));
        instance.setOrder(Integer.valueOf(2));
        instance.setDataDescription(mockInternationalString("es", "Descripción de Datos " + subCode + " en español", "en", "DataDescription " + subCode + " in English"));
        instance.setStatisticalPopulation(mockInternationalString("es", "Carga de Estadísticas " + subCode + " en español", "en", "StatisticalPopulation " + subCode + " in English"));
        instance.addStatisticalUnit(mockExternalItem("statisticalUnit1", TypeExternalArtefactsEnum.DATASTRUCTURE, "http://statisticalUnit1"));
        instance.addStatisticalUnit(mockExternalItem("statisticalUnit22", TypeExternalArtefactsEnum.DATASTRUCTURE, "http://statisticalUnit22"));
        instance.setGeographicGranularity(mockExternalItemBt("geographicGranularity", TypeExternalArtefactsEnum.CODELIST, "http://geographicGranularity"));
        instance.setGeographicComparability(mockInternationalString("es", "Comparando Geográficos " + subCode + " en español", "en", "geographicComparability " + subCode + " in English"));
        instance.setTemporalGranularity(mockExternalItemBt("temporalGranularity", TypeExternalArtefactsEnum.CODELIST, "http://temporalGranularity"));
        instance.setTemporalComparability(mockInternationalString("es", "Comparando Temporal " + subCode + " en español", "en", "temporalComparability " + subCode + " in English"));
        instance.setBasePeriod("2012");
        instance.addUnitMeasure(mockExternalItem("unitMeasure1", TypeExternalArtefactsEnum.CONCEPT, "http://unitMeasure1"));
        instance.setStatConcDef(mockInternationalString("es", "StatConcDef " + subCode + " en español", "en", "StatConcDef " + subCode + " in English"));
        instance.addStatConcDefList(mockExternalItem("statConcDefList1", TypeExternalArtefactsEnum.CODELIST, "http://statConcDefList1"));
        instance.addStatConcDefList(mockExternalItem("statConcDefList22", TypeExternalArtefactsEnum.CODELIST, "http://statConcDefList22"));
        instance.addStatConcDefList(mockExternalItem("statConcDefList333", TypeExternalArtefactsEnum.CODELIST, "http://statConcDefList333"));
        instance.setClassSystem(mockInternationalString("es", "ClassSystem " + subCode + " en español", "en", "ClassSystem " + subCode + " in English"));
        instance.addClassSystemList(mockExternalItem("statConcDefList1", TypeExternalArtefactsEnum.CODELIST, "http://statConcDefList1"));
        instance.addClassSystemList(mockExternalItem("statConcDefList22", TypeExternalArtefactsEnum.CODELIST, "http://statConcDefList22"));
        instance.setInstanceType(mockInstanceType("instanceType1"));
        instance.setInternalInventoryDate(new DateTime(2012, 12, 1, 13, 15, 14, 0));
        instance.setProcStatus(procStatus);
        instance.setDocMethod(mockInternationalString("es", "DocMethod " + subCode + " en español", "en", "DocMethod " + subCode + " in English"));
        instance.setSurveySource(mockSurveySource("surveySource1"));
        instance.setCollMethod(mockCollMethod("collMethod1"));
        instance.addInformationSupplier(mockExternalItem("informationSupplier1", TypeExternalArtefactsEnum.COMMON_METADATA, "http://informationSupplier1"));
        instance.addFreqColl(mockExternalItem("freqColl1", TypeExternalArtefactsEnum.CATEGORY_SCHEME, "http://freqColl1"));
        instance.addFreqColl(mockExternalItem("freqColl22", TypeExternalArtefactsEnum.CATEGORY_SCHEME, "http://freqColl22"));
        instance.setDataValidation(mockInternationalString("es", "DataValidation " + subCode + " en español", "en", "DataValidation " + subCode + " in English"));
        instance.setDataCompilation(mockInternationalString("es", "DataCompilation " + subCode + " en español", "en", "DataCompilation " + subCode + " in English"));
        instance.setAdjustment(mockInternationalString("es", "Adjustment " + subCode + " en español", "en", "Adjustment " + subCode + " in English"));
        instance.setCostBurden(mockInternationalString("es", "CostBurden " + subCode + " en español", "en", "CostBurden " + subCode + " in English"));
        instance.addCost(mockCost("cost1"));
        instance.addCost(mockCost("cost22"));
        instance.addCost(mockCost("cost333"));
        instance.addCost(mockCost("cost4444"));
        instance.setInventoryDate(new DateTime(2013, 2, 4, 13, 15, 14, 0));
        instance.setQualityDoc(mockInternationalString("es", "QualityDoc " + subCode + " en español", "en", "QualityDoc " + subCode + " in English"));
        instance.setQualityAssure(mockInternationalString("es", "QualityAssure " + subCode + " en español", "en", "QualityAssure " + subCode + " in English"));
        instance.setQualityAssmnt(mockInternationalString("es", "QualityAssmnt " + subCode + " en español", "en", "QualityAssmnt " + subCode + " in English"));
        instance.setUserNeeds(mockInternationalString("es", "UserNeeds " + subCode + " en español", "en", "UserNeeds " + subCode + " in English"));
        instance.setUserSat(mockInternationalString("es", "UserSat " + subCode + " en español", "en", "UserSat " + subCode + " in English"));
        instance.setCompleteness(mockInternationalString("es", "Completeness " + subCode + " en español", "en", "Completeness " + subCode + " in English"));
        instance.setTimeliness(mockInternationalString("es", "Timeliness " + subCode + " en español", "en", "Timeliness " + subCode + " in English"));
        instance.setPunctuality(mockInternationalString("es", "Punctuality " + subCode + " en español", "en", "Punctuality " + subCode + " in English"));
        instance.setAccuracyOverall(mockInternationalString("es", "AccuracyOverall " + subCode + " en español", "en", "AccuracyOverall " + subCode + " in English"));
        instance.setSamplingErr(mockInternationalString("es", "SamplingErr " + subCode + " en español", "en", "SamplingErr " + subCode + " in English"));
        instance.setNonsamplingErr(mockInternationalString("es", "NonsamplingErr " + subCode + " en español", "en", "NonsamplingErr " + subCode + " in English"));
        instance.setCoherXDomain(mockInternationalString("es", "CoherXDom " + subCode + " en español", "en", "CoherXDom " + subCode + " in English"));
        instance.setCoherInternal(mockInternationalString("es", "CoherInternal " + subCode + " en español", "en", "CoherInternal " + subCode + " in English"));
        instance.setComment(mockInternationalString("es", "Comentarios " + subCode + " en español", "en", "Comments " + subCode + " in English"));
        instance.setNotes(mockInternationalString("es", "Notas " + subCode + " en español", "en", "Notes " + subCode + " in English"));

        return instance;
    }
}