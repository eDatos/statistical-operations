package org.siemac.metamac.statistical.operations.core.serviceapi;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.Date;
import java.util.List;

import org.apache.commons.lang.RandomStringUtils;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.siemac.metamac.common.test.MetamacBaseTests;
import org.siemac.metamac.core.common.dto.serviceapi.ExternalItemBtDto;
import org.siemac.metamac.core.common.dto.serviceapi.InternationalStringDto;
import org.siemac.metamac.core.common.dto.serviceapi.LocalisedStringDto;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.schema.common.v1_0.domain.MetamacVersion;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.OperationDto;
import org.siemac.metamac.statistical.operations.core.enume.domain.StatusEnum;
import org.siemac.metamac.statistical.operations.internal.ws.v1_0.MetamacExceptionFault;
import org.siemac.metamac.statistical.operations.internal.ws.v1_0.MetamacStatisticalOperationsInternalInterfaceV10;
import org.siemac.metamac.statistical.operations.internal.ws.v1_0.domain.OperationBase;
import org.siemac.metamac.statistical.operations.internal.ws.v1_0.domain.OperationBaseList;
import org.siemac.metamac.statistical.operations.internal.ws.v1_0.domain.OperationCriteria;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * Spring based transactional test with DbUnit support.
 */

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:oracle/applicationContext-test.xml"})
public class StatisticalOperationsInternalWebserviceFacadeTest extends MetamacBaseTests {

    @Autowired
    protected StatisticalOperationsServiceFacade               statisticalOperationsServiceFacade;

    @Autowired
    protected MetamacStatisticalOperationsInternalInterfaceV10 metamacStatisticalOperationsInternalInterfaceV10;

    @Test
    public void testRetrieveVersion() throws MetamacExceptionFault {
        MetamacVersion metamacVersion = metamacStatisticalOperationsInternalInterfaceV10.retrieveVersion();
        assertEquals("v1_0", metamacVersion.getServiceVersion());
    }

    @Test
    public void testRetrieveOperation() throws Exception {

        // Create and publish
        OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing());
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationDto.getId());

        // Webservice: retrieve operartion
        OperationBase operationBase = metamacStatisticalOperationsInternalInterfaceV10.retrieveOperation(operationDto.getCode());

        assertNotNull(operationBase);
        assertEquals(operationDto.getCode(), operationBase.getCode());
        assertEquals(operationDto.getUri(), operationBase.getUri());
    }

    @Test
    public void testFindOperations() throws Exception {

        // Create and publish operation 1
        OperationDto operationDto1 = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing());
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationDto1.getId());
        statisticalOperationsServiceFacade.publishExternallyOperation(getServiceContext(), operationDto1.getId());
        // Create and publish operation 2
        OperationDto operationDto2 = statisticalOperationsServiceFacade.createOperation(getServiceContext(), createOperationDtoForInternalPublishing());
        statisticalOperationsServiceFacade.publishInternallyOperation(getServiceContext(), operationDto2.getId());

        // Webservice: Find operations
        OperationCriteria operationCriteria = new OperationCriteria();
        operationCriteria.setIsIndicatorsSystem(Boolean.TRUE);
        OperationBaseList operationBaseList = metamacStatisticalOperationsInternalInterfaceV10.findOperations(operationCriteria);

        assertNotNull(operationBaseList);
        boolean operation1Exists = false;
        boolean operation2Exists = false;
        for (OperationBase operationBase : operationBaseList.getOperation()) {
            if (operationBase.getCode().equals(operationDto1.getCode())) {
                operation1Exists = true;
            } else if (operationBase.getCode().equals(operationDto2.getCode())) {
                operation2Exists = true;
            }
        }
        assertTrue(operation1Exists);
        assertTrue(operation2Exists);
    }

    /**************************************************************************
     * DBUNIT CONFIGURATION
     **************************************************************************/

    private OperationDto createOperationDto() throws MetamacException {
        OperationDto operationDto = new OperationDto();

        // Auditable information
        operationDto.setCreatedBy("Junit");
        operationDto.setCreatedDate(new Date());

        // Identifier
        operationDto.setCode("PRUEBA01" + RandomStringUtils.random(50, true, true));

        // TITLE
        InternationalStringDto title = new InternationalStringDto();
        LocalisedStringDto title_es = new LocalisedStringDto();
        title_es.setLabel("Título en español de operacion");
        title_es.setLocale("es");
        LocalisedStringDto title_en = new LocalisedStringDto();
        title_en.setLabel("Título en inglés de operacion");
        title_en.setLocale("en");
        title.addText(title_es);
        title.addText(title_en);
        operationDto.setTitle(title);

        // DESCRIPTION
        InternationalStringDto acronym = new InternationalStringDto();
        LocalisedStringDto acronym_es = new LocalisedStringDto();
        acronym_es.setLabel("Descripción en español de operacion");
        acronym_es.setLocale("es");
        LocalisedStringDto acronym_en = new LocalisedStringDto();
        acronym_en.setLabel("Descripción en inglés de operacion");
        acronym_en.setLocale("en");
        acronym.addText(acronym_es);
        acronym.addText(acronym_en);
        operationDto.setAcronym(acronym);

        // RELEASE_CALENDAR
        operationDto.setReleaseCalendar(false);

        // RELEASE_CALENDAR_ACCESS
        operationDto.setReleaseCalendarAccess("http://www.draft.com");

        // SURVEY_TYPE
        operationDto.setSurveyType(statisticalOperationsServiceFacade.findSurveyTypeById(getServiceContext(), Long.valueOf(1)));

        // OFFICIALITY_TYPE
        operationDto.setOfficialityType(statisticalOperationsServiceFacade.findOfficialityTypeById(getServiceContext(), Long.valueOf(1)));

        // SUBJECT_AREA
        ExternalItemBtDto subjectArea = new ExternalItemBtDto();
        subjectArea.setCodeId("PRUEBA");
        subjectArea.setType(TypeExternalArtefactsEnum.CATEGORY);
        subjectArea.setUriInt("uri:internal:todo");
        operationDto.setSubjectArea(subjectArea);

        // STATUS
        operationDto.setStatus(StatusEnum.PLANNING);

        // INDICATOR_SYSTEM
        operationDto.setIndicatorSystem(true);

        return operationDto;
    }

    private OperationDto createOperationDtoForInternalPublishing() throws MetamacException {
        OperationDto operationDto = createOperationDto();

        // OBJECTIVE
        InternationalStringDto objective = new InternationalStringDto();
        LocalisedStringDto objective_es = new LocalisedStringDto();
        objective_es.setLabel("OPERACION - OBJECTIVE - ES");
        objective_es.setLocale("es");
        LocalisedStringDto objective_en = new LocalisedStringDto();
        objective_en.setLabel("OPERACION - OBJECTIVE - EN");
        objective_en.setLocale("en");
        objective.addText(objective_es);
        objective.addText(objective_en);
        operationDto.setObjective(objective);

        // SURVEY_TYPE
        operationDto.setSurveyType(statisticalOperationsServiceFacade.findSurveyTypeById(getServiceContext(), Long.valueOf(1)));

        // PRODUCER
        ExternalItemBtDto producer01 = new ExternalItemBtDto();
        producer01.setCodeId("ISTAC");
        producer01.setType(TypeExternalArtefactsEnum.AGENCY);
        producer01.setUriInt("uri:interna:todo");
        operationDto.addProducer(producer01);

        ExternalItemBtDto producer02 = new ExternalItemBtDto();
        producer02.setCodeId("INE");
        producer02.setType(TypeExternalArtefactsEnum.AGENCY);
        producer02.setUriInt("uri:interna:todo");
        operationDto.addProducer(producer02);

        // REGIONAL_RESPONSIBLE
        ExternalItemBtDto regionalResponsible01 = new ExternalItemBtDto();
        regionalResponsible01.setCodeId("ISTAC");
        regionalResponsible01.setType(TypeExternalArtefactsEnum.AGENCY);
        regionalResponsible01.setUriInt("uri:interna:todo");
        operationDto.addRegionalResponsible(regionalResponsible01);

        // PUBLISHER
        ExternalItemBtDto publisher01 = new ExternalItemBtDto();
        publisher01.setCodeId("ISTAC");
        publisher01.setType(TypeExternalArtefactsEnum.AGENCY);
        publisher01.setUriInt("uri:interna:todo");
        operationDto.addPublisher(publisher01);

        // COMMON_METADATA
        ExternalItemBtDto commonMetadata = new ExternalItemBtDto();
        commonMetadata.setCodeId("ISTAC");
        commonMetadata.setType(TypeExternalArtefactsEnum.AGENCY);
        commonMetadata.setUriInt("uri:interna:todo");
        operationDto.setCommonMetadata(commonMetadata);

        return operationDto;
    }

    @Override
    protected String getDataSetFile() {
        return "dbunit/StatisticalOperationsServiceFacadeTest.xml";
    }

    @Override
    protected List<String> getTablesToRemoveContent() {
        return null;
    }

    @Override
    protected List<String> getSequencesToRestart() {
        return null;
    }

    @Override
    public void tearDownDatabaseTester() throws Exception {
        // NOTHING;
    }
}